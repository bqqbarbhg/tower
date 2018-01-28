package util

/*

import scala.collection.mutable
import java.nio.ByteBuffer

class StringPoolBuilder {

  private val strings = new mutable.HashMap[String, Int]()

  def put(string: String): Unit = {
    strings.getOrElseUpdate(string, strings.size)
  }

  private case class StringRef(begin: Int, length: Int)

  def write(buf: ByteBuffer): Unit = {
    var megaString = ""
    val sorted = strings.toVector.sortBy(-_._1.length)
    val refs = new Array[StringRef](sorted.length)

    // Horrible O(n^2) but n is small-ish!
    for ((str, index) <- sorted) {
      var offset = megaString.indexOf(str)
      if (offset == -1) {
        offset = megaString.length
        megaString += str
      } else {
      }
      refs(index) = StringRef(offset, str.length)
    }

    val megaUtf8 = megaString.getBytes("UTF-8")

    val Version = 1
    buf.put(Version)
    buf.put(sorted.length)
    buf.put(megaUtf8.length)

    buf.put(megaUtf8)

    for (ref <- refs) {
      buf.put(ref.begin)
      buf.put(ref.length)
    }
  }

}

*/
