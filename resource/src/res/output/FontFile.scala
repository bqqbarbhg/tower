package res.output

import java.io.File
import java.nio.ByteBuffer

import core._
import org.lwjgl.system.MemoryUtil
import res.intermediate._
import util.BufferUtils._
import res.runner.OutputFileWriter

object FontFile {

  def save(writer: OutputFileWriter, file: File, font: BakedFont, imageRes: String): Unit = {
    // @Serialize(s2ft)

    val buffer = MemoryUtil.memAlloc(64 * 1024 * 1024)

    val Version = 1
    buffer.putMagic("s2ft")
    buffer.putVersion(Version)

    buffer.putIdentifier(imageRes)
    buffer.putInt(font.variants.length)
    buffer.putInt(font.kerningPairs.size)

    for (variant <- font.variants) {
      var flags = 0x00
      if (variant.config.signedDistanceField) flags |= 0x01

      buffer.putInt(flags)
      buffer.putInt(variant.height)
      buffer.putInt(variant.glyphs.size)
      for ((codepoint, rect) <- variant.glyphs) {
        buffer.putShort(codepoint.toShort)
        buffer.putShort(rect.channel.toShort)
        buffer.putShort(rect.rect.x.toShort)
        buffer.putShort(rect.rect.y.toShort)
        buffer.putShort(rect.rect.w.toShort)
        buffer.putShort(rect.rect.h.toShort)
        buffer.putFloat(rect.offset.x.toFloat)
        buffer.putFloat(rect.offset.y.toFloat)
      }
    }

    for ((pair, advance) <- font.kerningPairs) {
      buffer.putChar(pair._1)
      buffer.putChar(pair._2)
      buffer.putFloat(advance.toFloat)
    }

    buffer.putMagic("E.ft")
    buffer.finish()

    writer.writeFile(file, buffer)

    MemoryUtil.memFree(buffer)
  }
}
