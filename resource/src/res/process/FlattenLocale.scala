package res.process

import res.intermediate._
import io.SimpleSerialization._
import res.intermediate.FlatLocale._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Flatten a locale TOML-file to an efficent representation of the locale keys.
  * Also handles parsing and escaping of the template expressions.
  */
object FlattenLocale {

  private sealed abstract class ParsedPart
  private case class ParsedLiteral(text: String) extends ParsedPart
  private case class ParsedArgument(name: String) extends ParsedPart

  def flattenLocale(locale: Locale): FlatLocale = {
    val flat = new FlatLocale()

    val info = locale.map("Info").asInstanceOf[SMap]
    info.writeAll(flat.info)

    val strPoolMap = mutable.HashMap[String, Int]()

    def processKey(key: String, value: String): Unit = {

      val parts = ArrayBuffer[ParsedPart]()
      val builder = new StringBuilder()

      // Parse the value expression
      var ix = 0
      while (ix < value.length) {
        if (value(ix) == '{') {
          if (builder.nonEmpty) {
            parts += ParsedLiteral(builder.mkString)
            builder.clear()
          }
          ix += 1
          while (value(ix) != '}') {
            builder += value(ix)
            ix += 1
          }
          ix += 1
          assert(builder.nonEmpty, "Argument name must be non-empty")
          parts += ParsedArgument(builder.mkString)
          builder.clear()
        } else if (value(ix) == '\\') {
          ix += 1
          builder += (value(ix) match {
            case '\\' => '\\'
            case '{' => '{'
            case 'n' => '\n'
            case 't' => '\t'
            case other => throw new RuntimeException(s"Unknown escape: '$other'")
          })
          ix += 1
        } else {
          builder += value(ix)
          ix += 1
        }
      }
      if (builder.nonEmpty) {
        parts += ParsedLiteral(builder.mkString)
        builder.clear()
      }

      if (parts.length == 1 && parts.head.isInstanceOf[ParsedLiteral]) {
        flat.simpleKeys(key) = parts.head.asInstanceOf[ParsedLiteral].text
      } else if (parts.isEmpty) {
        flat.simpleKeys(key) = ""
      } else {
        val argNames = ArrayBuffer[String]()
        val flatParts = parts.map(part => {
          part match {
            case ParsedLiteral(str) =>
              val index = strPoolMap.getOrElseUpdate(str, {
                flat.expressionStringPool += str
                flat.expressionStringPool.length - 1
              })
              Literal(index)
            case ParsedArgument(name) =>
              val index = argNames.indexOf(name) match {
                case -1 =>
                  argNames += name
                  argNames.length - 1
                case ix => ix
              }
              Argument(index)
          }
        })
        flat.expressionKeys(key) = Expression(flatParts.toVector, argNames.toVector)
      }
    }

    def visitLocale(prefix: String, map: SMap): Unit = {
      for ((key, value) <- map.pairs) {
        val fullKey = prefix + key
        value match {
          case map: SMap => visitLocale(fullKey + ".", map)
          case SString(str) => processKey(fullKey, str)
          case other => throw new RuntimeException(s"Unexpected type in locale at '$fullKey': ${other.kind}")
        }
      }
    }

    val root = locale.map("Locale").asInstanceOf[SMap]
    visitLocale("", root)

    flat
  }

}

