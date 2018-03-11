package locale

import collection.mutable.ArrayBuffer
import java.nio.ByteBuffer

import core._
import org.lwjgl.system.MemoryUtil
import util.BufferUtils._
import io.content.Package

object Locale {

  var instance: Locale = null

  def getSimple(index: Int): String = instance.getSimple(index)
  def getExpression(index: Int, args: Array[String]): String = instance.getExpression(index, args)

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

  private var simple = Array[String]()

  private var expressionStringPool = Array[String]()
  private var expressionData = Array[Int]()
  private var expressionBeginEnd = Array[Int]()

  def getSimple(index: Int): String = simple(index)

  def getExpression(index: Int, args: Array[String]): String = {
    val builder = new StringBuilder()
    var ix = expressionBeginEnd(index * 2 + 0)
    var end = expressionBeginEnd(index * 2 + 1)
    while (ix < end) {
      val part = expressionData(ix)
      if (part >= 0)
        builder ++= expressionStringPool(part)
      else
        builder ++= args(-part - 1)
      ix += 1
    }
    builder.mkString
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

    simple = Array.fill(LocaleGetter.simpleLocales.size)("")
    expressionBeginEnd = new Array[Int](2 * LocaleGetter.simpleLocales.size)
    expressionStringPool = new Array[String](numPool)

    for (i <- 0 until numSimple) {
      val key = buffer.getString()
      val value = buffer.getString()
      for (index <- LocaleGetter.simpleLocales.get(key)) {
        simple(index) = value
      }
    }

    val exprData = new ArrayBuffer[Int]()
    for (i <- 0 until numExpr) {
      val key = buffer.getString()
      val numArgs = buffer.getInt()
      val numParts = buffer.getInt()
      val argNames = Array.fill(numArgs)(buffer.getString())
      val parts = Array.fill(numParts)(buffer.getInt())

      for (index <- LocaleGetter.expressionLocales.get(key)) {
        val refNames = LocaleGetter.expressionArgNames(index)
        val base = exprData.length
        expressionBeginEnd(index * 2 + 0) = base

        for (part <- parts) {
          if (part >= 0) {
            exprData += part
          } else {
            val name = argNames(-part - 1)
            val argIx = refNames.indexOf(name)
            if (argIx >= 0) {
              exprData += -argIx - 1
            }
          }
        }

        expressionBeginEnd(index * 2 + 1) = exprData.length
      }
    }
    expressionData = exprData.toArray

    for (i <- 0 until numPool) {
      expressionStringPool(i) = buffer.getString()
    }

    buffer.verifyMagic("E.lc")
  }

}
