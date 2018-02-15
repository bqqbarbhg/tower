package io

import java.nio.CharBuffer

import Toml._

import scala.collection.mutable
import scala.util.matching.Regex
import scala.reflect.ClassTag

class TomlParseException(val message: String) extends RuntimeException(message)

import io.SimpleSerialization._

/**
  * An extremely hacky TOML implementation.
  */
object Toml {

  private val TokSpace = raw"[ \t]+".r
  private val TokNewline = raw"(\n|\r\n)".r
  private val TokBare = raw"[A-Za-z0-9_\-]+".r
  private val TokBasicString = "\"([^\\\"]|\\.)*\"".r
  private val TokLiteralString = raw"'[^']*'".r
  private val TokInt = raw"[+-]?[0-9_]+".r
  private val TokFloat = raw"[+-]?[0-9_]*\.[0-9_]+([eE][+-][0-9]+)?".r

  private case class Table(data: mutable.HashMap[String, Any] = new mutable.HashMap[String, Any](),
                           order: mutable.ArrayBuffer[String] = new mutable.ArrayBuffer[String]()) {

    def getOrElseUpdate(key: String, value: => Any): Any = {
      data.getOrElseUpdate(key, {
        order += key
        value
      })
    }

  }
  private type TableArray = mutable.ArrayBuffer[Table]

  private class Parser(source: String, filename: String) {
    val sequence = CharBuffer.wrap(source)
    var position: Int = 0

    val root = new Table()
    var context = root

    def seq: CharSequence = sequence.subSequence(position, sequence.length)

    def findLocation: (Int, Int) = {
      var pos = 0
      var line = 1
      var col = 1
      while (pos < position) {
        if (source(pos) == '\n') {
          line += 1
          col = 1
        } else {
          col += 1
        }
        pos += 1
      }
      (line, col)
    }

    def error(message: String): Nothing = {
      val (line, col) = findLocation
      throw new TomlParseException(s"$filename:$line:$col: TOML error: $message")
    }

    def atEnd: Boolean = position == source.length

    def peek(regex: Regex): Boolean = {
      regex.findPrefixOf(seq).isDefined
    }

    def peek(str: String): Boolean = {
      source.startsWith(str, position)
    }

    def eat(regex: Regex): String = {
      regex.findPrefixOf(seq) match {
        case Some(m) =>
          position += m.length
          m
        case None => ""
      }
    }

    def eat(str: String): String = {
      if (peek(str)) {
        position += str.length
        str
      } else {
        ""
      }
    }

    def space(): Unit = eat(TokSpace)

    def endLine(): Unit = {
      space()
      if (eat("#").nonEmpty) {
        do {
          position += 1
        } while (!atEnd && !peek(TokNewline))
      }
      if (!atEnd && eat(TokNewline).isEmpty) {
        println(position)
        println(source.length)
        error(s"Expected newline at end of line, got '${source(position)}'")
      }
    }

    def parseKeyPart(): String = {
      var str = eat(TokBare)
      if (str.nonEmpty) return str
      str = eat(TokBasicString)
      if (str.nonEmpty) return str.substring(1, str.length - 1)
      str = eat(TokLiteralString)
      if (str.nonEmpty) return str.substring(1, str.length - 1)
      error("Expected key")
    }

    def parseKey(): Vector[String] = {
      var res = Vector(parseKeyPart())
      while (eat(".").nonEmpty) {
        res :+= parseKeyPart()
      }
      res
    }

    def parseValue(): Any = {
      var str = eat(TokBasicString)
      if (str.nonEmpty) return SString(str.substring(1, str.length - 1))
      str = eat(TokLiteralString)
      if (str.nonEmpty) return SString(str.substring(1, str.length - 1))
      str = eat(TokFloat)
      if (str.nonEmpty) return SFloat(str.replace("_", "").toDouble)
      str = eat(TokInt)
      if (str.nonEmpty) return SInt(str.replace("_", "").toInt)
      str = eat("true")
      if (str.nonEmpty) return SBool(true)
      str = eat("false")
      if (str.nonEmpty) return SBool(false)
      error("Expected a value")
    }

    def set[T: ClassTag](base: Table, key: Vector[String], value: => T, force: Boolean = false): T = {
      var map = base
      for (k <- key.dropRight(1)) {
        val child = map.getOrElseUpdate(k, new Table())
        map = child match {
          case t: Table => t
          case _ => error(s"Expected a table for key '${key.mkString(".")}'")
        }
      }

      val ret = {
        if (force) {
          if (map.data.contains(key.last)) error(s"Key '${key.mkString(".")} is defined already")
          val v = value
          map.data(key.last) = v
          map.order += key.last
          v
        } else {
          map.getOrElseUpdate(key.last, value)
        }
      }

      ret match {
        case t: T => t
        case _ => error(s"Type mismatch for '${key.mkString(".")}'")
      }
    }

    def parseLine(): Unit = {
      space()

      if (eat("[[").nonEmpty) {
        val array = set(root, parseKey(), new TableArray())
        val table = new Table()
        array += table
        context = table
        if (eat("]]").isEmpty) error("Expected closing ']]'")
      } else if (eat("[").nonEmpty) {
        context = set(root, parseKey(), new Table())
        if (eat("]").isEmpty) error("Expected closing ']'")
      } else if (peek("#")) {
        endLine()
      } else if (peek(TokNewline) || atEnd) {
        endLine()
      } else {
        val key = parseKey()
        space()
        if (eat("=").isEmpty) error("Expected '='")
        space()
        val value = parseValue()
        endLine()

        set(context, key, value, true)
      }
    }

    def parse(): Unit = {
      while (!atEnd)
        parseLine()
    }

    def convertValue(v: Any): SValue = {
      v match {
        case t: Table => new SMap(t.data.mapValues(convertValue).toMap, t.order.toVector)
        case t: mutable.ArrayBuffer[_] => new SArray(t.asInstanceOf[TableArray].map(convertValue).toVector)
        case v: SValue => v
        case _ => error("Internal error: Something weird got into the data")
      }
    }

  }

  def parse(source: String, filename: String = ""): SMap = {
    val parser = new Parser(source, filename)
    parser.parse()
    parser.convertValue(parser.root).asInstanceOf[SMap]
  }

  def parseFile(filename: String): SMap = {
    val source = scala.io.Source.fromFile(filename)
    val str = source.mkString
    source.close
    parse(str, filename)
  }

  private def formatImpl(builder: mutable.StringBuilder, context: String, map: SMap, inArray: Boolean, arrayContext: String): Unit = {
    val all = map.pairs
    val simple = all.filter({ case (k, v) =>
        v match {
          case _: SMap => false
          case _: SArray => false
          case _ => true
        }
    })
    val maps = all.filter({ case (k, v) =>
      v match {
        case _: SMap => true
        case _ => false
      }
    })
    val arrays = all.filter({ case (k, v) =>
      v match {
        case _: SArray => true
        case _ => false
      }
    })

    if (simple.nonEmpty && context.nonEmpty && !inArray) {
      builder ++= s"\n[${context.dropRight(1)}]\n"
    }

    for ((k, v) <- simple) {
      builder ++= s"$arrayContext$k = "
      builder ++= (v match {
        case SString(s) => s""""$s""""
        case SInt(i) => i.toString
        case SFloat(f) => f.toString
        case SBool(b) => if (b) "true" else "false"
        case _ => throw new AssertionError("Type not in expected set")
      })
      builder ++= "\n"
    }

    for ((k, v) <- maps) {
      if (!inArray) {
        val newCtx = s"$context$k."
        formatImpl(builder, newCtx, v.asInstanceOf[SMap], false, "")
      } else {
        val arrayCtx = s"$arrayContext$k."
        formatImpl(builder, context, v.asInstanceOf[SMap], true, arrayCtx)
      }
    }

    for ((k, vs) <- arrays) {
      assert(!inArray, "Nested arrays not supported")
      val vvs = vs.asInstanceOf[SArray]
      for (v <- vvs.data) {
        builder ++= s"\n[[$context$k]]\n"
        val newCtx = s"$context$k."
        formatImpl(builder, newCtx, v.asInstanceOf[SMap], true, "")
      }
    }
  }

  def format(root: SMap): String = {
    val builder = new mutable.StringBuilder()
    formatImpl(builder, "", root, false, "")
    builder.mkString
  }
}
