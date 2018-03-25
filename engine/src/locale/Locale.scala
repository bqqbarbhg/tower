package locale

import collection.mutable.{ArrayBuffer, HashMap}
import java.nio.ByteBuffer

import core._
import org.lwjgl.system.MemoryUtil
import util.BufferUtils._
import io.content.Package

object Locale {

  var instance: Locale = null

  def getSimple(key: String): String = instance.getSimple(key)
  def getSimpleOption(key: String): Option[String] = instance.getSimpleOption(key)
  def getExpression(key: String, args: Map[String, String]): String = instance.getExpression(key, args)
  def getExpression(key: String, args: (String, String)*): String = instance.getExpression(key, args.toMap)
  def getExpressionOption(key: String, args: Map[String, String]): Option[String] = instance.getExpressionOption(key, args)
  def getExpressionOption(key: String, args: (String, String)*): Option[String] = instance.getExpressionOption(key, args.toMap)

  def load(info: LocaleInfo): Unit = load(info.file)
  def load(filename: Identifier): Unit = {
    instance = new Locale(filename)
    val file = Package.get.get(filename).get
    val buffer = MemoryUtil.memAlloc(file.sizeInBytes.toInt)
    val stream = file.read()
    buffer.readFrom(stream)
    buffer.finish()
    instance.load(buffer)
    stream.close()
    MemoryUtil.memFree(buffer)
  }

}

class Locale(val filename: Identifier) {

  private val argNameList = new ArrayBuffer[String]()
  private val argNameOrder = new HashMap[String, Int]()

  private var simple = HashMap[String, String]()

  private var expressionStringPool = Array[String]()
  private var expressionData = Array[Int]()
  private var expressionBeginEnd = HashMap[String, (Int, Int)]()

  def getSimple(key: String): String = simple.get(key).getOrElse(s"<$key>")
  def getSimpleOption(key: String): Option[String] = simple.get(key)

  def getExpression(key: String, args: Map[String, String]): String = getExpressionOption(key, args).getOrElse(s"<$key>")
  def getExpressionOption(key: String, args: Map[String, String]): Option[String] = {
    val maybeExp = for ((begin, end) <- expressionBeginEnd.get(key)) yield {
      var ix = begin
      val builder = new StringBuilder()
      while (ix < end) {
        val part = expressionData(ix)
        if (part >= 0) {
          builder ++= expressionStringPool(part)
        } else {
          val index = -part - 1
          builder ++= args.getOrElse(argNameList(index), s"<{${argNameList(index)}}>")
        }
        ix += 1
      }
      builder.result
    }

    maybeExp.orElse(getSimpleOption(key))
  }

  def load(buffer: ByteBuffer): Unit = {
    buffer.verifyMagic("s2lc")
    val MaxVersion = 1
    val version = buffer.getVersion(MaxVersion)

    // Skip the info
    buffer.verifyMagic("s2li")
    buffer.getString()
    buffer.getString()
    buffer.verifyMagic("E.li")

    val numSimple = buffer.getInt()
    val numExpr = buffer.getInt()
    val numPool = buffer.getInt()

    expressionStringPool = new Array[String](numPool)

    for (i <- 0 until numSimple) {
      val key = buffer.getString()
      val value = buffer.getString()
      simple(key) = value
    }

    val exprData = new ArrayBuffer[Int]()
    for (i <- 0 until numExpr) {
      val key = buffer.getString()
      val numArgs = buffer.getInt()
      val numParts = buffer.getInt()
      val argNames = Array.fill(numArgs)(buffer.getString())
      val parts = Array.fill(numParts)(buffer.getInt())

      val base = exprData.length
      val begin = base

      for (part <- parts) {
        if (part >= 0) {
          exprData += part
        } else {
          val name = argNames(-part - 1)
          val argIx = argNameOrder.getOrElseUpdate(name, {
            argNameList += name
            argNameOrder.size
          })
          if (argIx >= 0) {
            exprData += -argIx - 1
          }
        }
      }

      val end = exprData.length
      expressionBeginEnd(key) = (begin, end)
    }
    expressionData = exprData.toArray

    for (i <- 0 until numPool) {
      expressionStringPool(i) = buffer.getString()
    }

    buffer.verifyMagic("E.lc")
  }

}
