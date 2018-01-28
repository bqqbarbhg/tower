package util

/*

import java.nio.ByteBuffer

class StringPool(buffer: ByteBuffer) {

  private var strings: Array[String] = null

  {
    val MaxVersion = 1
    val version = buffer.getInt()
    if (version > MaxVersion) throw DeserializationException(s"Unsupported string pool version $version, expected $MaxVersion")

    val numStrings = buffer.getInt()
    val megaUtf8Length = buffer.getInt()

    val megaUtf8 = new Array[Byte](megaUtf8Length)
    buffer.get(megaUtf8)
    val megaString = new String(megaUtf8, "UTF-8")

    strings = new Array[String](numStrings)
    for (i <- 0 until strings.length) {
      val offset = buffer.getInt()
      val length = buffer.getInt()
      strings(i) = megaString.substring(offset, offset + length)
    }
  }

  def get(index: Int): String = strings(index)
}

*/
