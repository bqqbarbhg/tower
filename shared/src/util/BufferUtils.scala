package util

import java.io.{File, FileInputStream, FileOutputStream, InputStream, OutputStream}
import java.nio.ByteBuffer

import core._

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
      val chunk = new Array[Byte](4096)

      var toRead = math.min(chunk.length, math.max(buffer.remaining, 1))
      var num = stream.read(chunk, 0, toRead)
      while (num > 0) {
        buffer.put(chunk, 0, toRead)
        toRead = math.min(chunk.length, math.max(buffer.remaining, 1))
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
        val toWrite = math.min(chunk.length, num)
        buffer.get(chunk, 0, toWrite)
        stream.write(chunk, 0, toWrite)
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

    /** Writes a Matrix43 with full precision */
    def putMatrix43(m: Matrix43): Unit = {
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
    }

    /** Reads a Matrix43 with full precision */
    def getMatrix43(): Matrix43 = {
      val m = new Matrix43
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
      m
    }

  }
}

