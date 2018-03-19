package process

import java.nio.file.Paths

import io.SimpleSerialization._
import res.intermediate._

import scala.io.Source

object HyphenateLocale {

  val WordRegex = "\\p{L}+".r

  private def insertSoftHyphens(text: String, hyphenateWord: String => String): String = {
    WordRegex.replaceAllIn(text, m => {
      val word = m.group(0)
      hyphenateWord(m.group(0))
    })
  }

  private def insertSoftHyphens(map: SMap, hyphenateWord: String => String): SMap = {
    var newMap = map
    for ((key, value) <- map.pairs) {
      value match {
        case SString(str) =>
          newMap = newMap.updated(key, SString(insertSoftHyphens(str, hyphenateWord)))
        case child: SMap =>
          newMap = newMap.updated(key, insertSoftHyphens(child, hyphenateWord))
        case _ =>
      }
    }
    newMap
  }

  private def insertSoftHyphens(locale: Locale, hyphenateWord: String => String): Unit = {
    locale.map("Locale") match {
      case locales: SMap =>
        locale.map = locale.map.updated("Locale", insertSoftHyphens(locales, hyphenateWord))
      case _ => // Nop
    }
  }

  private def hyphenateLocaleDatabase(locale: Locale, dataRoot: String): Unit = {
    val hyphenDb = locale.map.find("Info.Hyphenation.database").asInstanceOf[SString].v
    val hyphenFile = Paths.get(dataRoot, hyphenDb).toFile
    if (hyphenFile.exists()) {
      val source = Source.fromFile(hyphenFile)(scala.io.Codec.UTF8)
      val lines = source.getLines().toVector
      source.close()

      val hyphenDict = lines.flatMap(line => {
        val parts = line.split(' ')
        if (parts.length >= 2) {
          val word = parts(0).toLowerCase
          val hyphens = parts(1).stripSuffix(";")
          val shys = hyphens.replace('-', '\u00AD')
          Some((word, shys))
        } else {
          None
        }
      }).toMap

      def hyphenateWord(word: String): String = {
        hyphenDict.get(word.toLowerCase) match {
          case Some(hyphens) =>
            var srcIx = 0
            var result = new StringBuilder()
            for (ch <- hyphens) {
              if (ch != '\u00AD') {
                result += word(srcIx)
                srcIx += 1
              } else {
                result += '\u00AD'
              }
            }
            result.result
          case None => word
        }
      }

      insertSoftHyphens(locale, hyphenateWord(_))
    }
  }

  // Based on: https://github.com/vepasto/finnish-hyphenator
  private def hyphenateLocaleFinnish(locale: Locale, dataRoot: String): Unit = {
    val vw = "aeiouyåäö"
    val cn = "bcdfghjklmnpqrstvwxyz"
    val HyphenRegex = s"(?i)([$vw$cn]{2})([$cn][$vw])".r
    def hyphenateWord(word: String): String = {
      var a = word
      for (i <- 0 until 10) {
        a = HyphenRegex.replaceAllIn(a, m => m.group(1) + '\u00AD' + m.group(2))
      }
      a
    }

    insertSoftHyphens(locale, hyphenateWord(_))
  }

  def hyphenateLocale(locale: Locale, dataRoot: String): Unit = {

    val algorithm = locale.map.find("Info.Hyphenation.algorithm")

    algorithm match {
      case SString("database") => hyphenateLocaleDatabase(locale, dataRoot)
      case SString("finnish") => hyphenateLocaleFinnish(locale, dataRoot)
      case _ =>
    }

  }

}

