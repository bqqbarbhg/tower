package tower.util

import java.nio.{ByteBuffer, ByteOrder}
import java.io.{InputStream, OutputStream}

import tower.Identifier
import tower.math.Matrix4

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
      if (ref.deep != chars.deep) throw SerializationException(s"Invalid magic, expected $magic!")
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
      val copy = buffer.duplicateEx()
      copy.position(0)
      while (copy.position < buffer.position) {
        val toCopy = scala.math.min(chunk.size, buffer.position - copy.position)
        copy.get(chunk, 0, toCopy)
        stream.write(chunk, 0, toCopy)
      }
    }

    /** Reads the contents of a stream to this buffer and resets the position, returns the number of bytes read */
    def readFrom(stream: InputStream): Int = {
      val chunk = new Array[Byte](64 * 1024)
      buffer.position(0)
      var num = stream.read(chunk)
      while (num > 0) {
        buffer.put(chunk, 0, num)
        num = stream.read(chunk)
      }
      val size = buffer.position
      buffer.position(0)
      size
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

    /** Writes a Matrix4 with full precision */
    def putMatrix4(m: Matrix4): Unit = {
      buffer.putDouble(m.m11)
      buffer.putDouble(m.m12)
      buffer.putDouble(m.m13)
      buffer.putDouble(m.m14)
      buffer.putDouble(m.m21)
      buffer.putDouble(m.m22)
      buffer.putDouble(m.m23)
      buffer.putDouble(m.m24)
      buffer.putDouble(m.m31)
      buffer.putDouble(m.m32)
      buffer.putDouble(m.m33)
      buffer.putDouble(m.m34)
      buffer.putDouble(m.m41)
      buffer.putDouble(m.m42)
      buffer.putDouble(m.m43)
      buffer.putDouble(m.m44)
    }

    /** Reads a Matrix4 with full precision */
    def getMatrix4(): Matrix4 = {
      val m = new Matrix4
      m.m11 = buffer.getDouble()
      m.m12 = buffer.getDouble()
      m.m13 = buffer.getDouble()
      m.m14 = buffer.getDouble()
      m.m21 = buffer.getDouble()
      m.m22 = buffer.getDouble()
      m.m23 = buffer.getDouble()
      m.m24 = buffer.getDouble()
      m.m31 = buffer.getDouble()
      m.m32 = buffer.getDouble()
      m.m33 = buffer.getDouble()
      m.m34 = buffer.getDouble()
      m.m41 = buffer.getDouble()
      m.m42 = buffer.getDouble()
      m.m43 = buffer.getDouble()
      m.m44 = buffer.getDouble()
      m
    }

    /** Writes a identifier with leading size and UTF-8 encoded content padded to 4-byte boundary */
    def putIdentifier(str: String): Unit = putString(str)
    /** Reads a identifier with leading size and UTF-8 encoded content padded to 4-byte boundary */
    def getIdentifier(): Identifier = Identifier(buffer.getString())

    /** For some reason JVM forgets byte order when duplicating !?! */
    def duplicateEx(): ByteBuffer = buffer.duplicate.order(buffer.order())

  }

}
