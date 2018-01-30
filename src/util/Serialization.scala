package tower.util

import java.nio.ByteBuffer
import java.io.OutputStream

import tower.Identifier

case class SerializationException(msg: String) extends Exception(msg)

object Serialization {

  implicit class ByteBufferExtension(val buffer: ByteBuffer) extends AnyVal {

    /** Writes a 4-byte ASCII magic number */
    def putMagic(magic: String): Unit = {
      val chars = magic.map(_.toByte).toArray
      buffer.put(chars)
    }

    /** Verifies a 4-byte ASCII magic number */
    def verifyMagic(magic: String): Unit = {
      val ref = magic.map(_.toByte).toArray
      val chars = new Array[Byte](4)
      buffer.get(chars)
      if (ref != chars) throw SerializationException(s"Invalid magic, expected $magic!")
    }

    /** Writes version number as 32-bit integer */
    def putVersion(version: Int): Unit = buffer.putInt(version)

    /** Reads version number as 32-bit integer and checks support */
    def getVersion(maxVersion: Int, minVersion: Int = 1): Int = {
      val version = buffer.getInt()
      if (version < minVersion || version > maxVersion)
        throw SerializationException(s"Unsupported version, expected between: $minVersion - $maxVersion")
      version
    }

    /** Writes the contents of this buffer (up to current position) to a stream. */
    def writeTo(stream: OutputStream): Unit = {
      val chunk = new Array[Byte](64 * 1024)
      val copy = buffer.duplicate()
      copy.position(0)
      while (copy.position < buffer.position) {
        val toCopy = scala.math.min(chunk.size, buffer.position - copy.position)
        copy.get(chunk, 0, toCopy)
        stream.write(chunk, 0, toCopy)
      }
    }

    /** Writes a string with leading size and UTF-8 encoded content padded to 4-byte boundary */
    def putString(str: String): Unit = {
      val utf8 = str.getBytes("UTF-8")
      val pad = (4 - (utf8.length + 2) % 4) % 4
      buffer.putShort(str.length.toShort)
      buffer.put(utf8)
      for (p <- 0 until pad) buffer.put(0.toByte)
    }

    /** Reads a string with leading size and UTF-8 encoded content padded to 4-byte boundary */
    def getString(): String = {
      val length = buffer.getShort()
      val pad = (4 - (length + 2) % 4) % 4
      val utf8 = new Array[Byte](length + pad)
      buffer.get(utf8)
      new String(utf8, 0, length, "UTF-8")
    }

    /** Writes a identifier with leading size and UTF-8 encoded content padded to 4-byte boundary */
    def putIdentifier(str: String): Unit = putString(str)
    /** Reads a identifier with leading size and UTF-8 encoded content padded to 4-byte boundary */
    def getIdentifier(): Identifier = Identifier(buffer.getString())

  }

}
