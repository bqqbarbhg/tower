package core

import scala.collection.mutable.HashMap

object Identifier {
  val Empty = new Identifier(0)

  private val symbols = HashMap[String, Int]("" -> 0)
  private var strings = Array[String]("")

  private def intern(str: String): Int = {
    Identifier.synchronized {
      symbols.get(str) match {
        case Some(index) => index
        case None =>
          val index = symbols.size
          symbols(str) = index
          if (index == strings.length) {
            val oldStrings = strings
            strings = new Array[String](strings.length * 2)
            java.lang.System.arraycopy(oldStrings, 0, strings, 0, oldStrings.length)
          }
          strings(index) = str
          index
      }
    }
  }

  def apply(str: String): Identifier = new Identifier(intern(str))
}

/**
  * A lightweight permanent interned string.
  */
class Identifier(val index: Int) extends AnyVal {
  override def toString: String = {
    Identifier.synchronized {
      Identifier.strings(index)
    }
  }
}

