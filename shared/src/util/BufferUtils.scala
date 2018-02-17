package util

import java.io.{File, FileInputStream, FileOutputStream, InputStream, OutputStream}
import java.nio.ByteBuffer

case class BufferIntegrityException(val message: String) extends RuntimeException(message)

/** Contains nice-to-have things for manipulating ByteBuffers */
object BufferUtils {
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
      if (ref.deep != chars.deep) throw BufferIntegrityException(s"Invalid magic, expected $magic!")
    }

    /** Reads a 4-byte ASCII magic number */
    def getMagic(): String = {
      val chars = new Array[Byte](4)
      buffer.get(chars)
      new String(chars.map(_.toChar))
    }

    /** Writes version number as 32-bit integer */
    def putVersion(version: Int): Unit = buffer.putInt(version)

    /** Reads version number as 32-bit integer and checks support */
    def getVersion(maxVersion: Int, minVersion: Int = 1): Int = {
      val version = buffer.getInt()
      if (version < minVersion || version > maxVersion)
        throw BufferIntegrityException(s"Unsupported version, expected between: $minVersion - $maxVersion")
      version
    }

    /** Read the contents from a stream at the current position */
    def readFrom(stream: InputStream): Unit = {
      val left = buffer.remaining
      assert(left > 0)
      val chunk = new Array[Byte](math.min(4096, left))

      var num = stream.read(chunk, 0, chunk.length)
      while (num > 0) {
        buffer.put(chunk, 0, chunk.length)
        num = stream.read(chunk, 0, chunk.length)
      }
    }

    /** Read the contents from a file */
    def readFromFile(file: File): Unit = {
      val stream = new FileInputStream(file)
      readFrom(stream)
      stream.close()
    }

    /** Read the contents from a file */
    def readFromFile(file: String): Unit = readFromFile(new File(file))

    /** Write the contents from position to limit to stream */
    def writeTo(stream: OutputStream): Unit = {
      val left = buffer.remaining
      assert(left > 0)
      val chunk = new Array[Byte](math.min(4096, left))
      val begin = buffer.position

      var num = buffer.limit - buffer.position
      while (num > 0) {
        buffer.get(chunk, 0, num)
        stream.write(chunk, 0, num)
        num = buffer.limit - buffer.position
      }

      buffer.position(begin)
    }
    /** Write the contents (up to the limit) to a file */
    def writeToFile(file: File): Unit = {
      val stream = new FileOutputStream(file)
      writeTo(stream)
      stream.close()
    }

    /** Write the contents (up to the limit) to a file */
    def writeToFile(file: String): Unit = writeToFile(new File(file))

  }
}

