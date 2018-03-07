package res.output

import java.io.File
import java.nio.ByteBuffer

import core._
import org.lwjgl.system.MemoryUtil
import res.intermediate.FlatLocale._
import res.intermediate._
import util.BufferUtils._
import res.runner.OutputFileWriter

object LocaleFile {

  def save(writer: OutputFileWriter, file: File, locale: FlatLocale): Unit = {
    // @Serialize(s2lc)

    val buffer = MemoryUtil.memAlloc(64 * 1024 * 1024)

    val Version = 1
    buffer.putMagic("s2lc")
    buffer.putVersion(Version)

    val sortedSimpleKeys = locale.simpleKeys.toSeq.sortBy(_._1)
    val sortedExprKeys = locale.expressionKeys.toSeq.sortBy(_._1)

    buffer.putInt(sortedSimpleKeys.size)
    buffer.putInt(sortedExprKeys.size)
    buffer.putInt(locale.expressionStringPool.length)

    buffer.putIdentifier(locale.info.code)
    buffer.putString(locale.info.language)

    for ((key, value) <- sortedSimpleKeys) {
      buffer.putString(key)
      buffer.putString(value)
    }

    for ((key, expr) <- sortedExprKeys) {
      buffer.putString(key)
      buffer.putInt(expr.argNames.length)
      buffer.putInt(expr.parts.length)
      for (name <- expr.argNames)
        buffer.putString(name)
      for (part <- expr.parts) {
        // Pack literals 0,1,2... and arguments -1,-2,-3...
        val value = part match {
          case Literal(index) => index
          case Argument(index) => -(index + 1)
        }
        buffer.putInt(value)
      }
    }

    for (str <- locale.expressionStringPool) {
      buffer.putString(str)
    }

    buffer.putMagic("E.lc")
    buffer.finish()

    writer.writeFile(file, buffer)

    MemoryUtil.memFree(buffer)
  }
}

