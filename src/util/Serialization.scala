package util

/*

import java.nio.ByteBuffer

case class SerializationException(message: String) extends Exception(message)
case class DeserializationException(message: String) extends Exception(message)

object Serialization {

  private def safeAscii(str: String): String = {
    str.map(c => {
      if (c >= ' ' && c <= '~') c
      else f"\\x${c.toInt}%04x"
    })
  }

  implicit class ExtendedByteBuffer(buf: ByteBuffer) {

    /**
      * Write a magic number to the header
      * @param magic 4 character ASCII code
      */
    def putMagic(magic: String): Unit = {
      if (magic.length != 4) {
        throw SerializationException(s"Expected magic to have 4 characters, got ${magic.length}")
      }

      buf.put(magic.map(_.toChar).toArray)
    }

    /**
      * Verify a magic number in the header
      * @param magic Expected 4 character ASCII code
      */
    def verifyMagic(magic: String): Unit = {
      if (magic.length != 4) {
        throw DeserializationException(s"Expected reference magic to have 4 characters, got ${magic.length}")
      }

      val ref = new Array[Byte](4)
      buf.get(ref)

      val refS = ref.map(_.toChar).toString
      if (ref != refS) {
        throw DeserializationException(s"Magic '${safeAscii(refS)}' doesn't match expected '$magic'")
      }
    }

  }

}

*/
