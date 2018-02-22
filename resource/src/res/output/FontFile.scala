package res.output

import java.io.File
import java.nio.ByteBuffer

import core._
import org.lwjgl.system.MemoryUtil
import res.intermediate._
import util.BufferUtils._
import res.runner.OutputFileWriter

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object FontFile {

  def save(writer: OutputFileWriter, file: File, font: BakedFont, imageRes: String): Unit = {

    val kerningMap = new mutable.HashMap[Char, ArrayBuffer[(Char, Double)]]()
    for ((pair, advance) <- font.kerningPairs) {
      val (prev, next) = pair
      val kerns = kerningMap.getOrElseUpdate(next, new ArrayBuffer[(Char, Double)]())
      kerns.append((prev, advance))
    }

    val kernDataSize = kerningMap.values.map(_.length).sum

    val kernPrevChar = new ArrayBuffer[Int]()
    val kernAmount = new ArrayBuffer[Double]()

    val kerningOrder = kerningMap.toSeq.sortBy(_._1)
    val kerningOffsets = (for ((next, kerns) <- kerningOrder) yield {
      val offset = kernPrevChar.length
      val sortedKerns = kerns.sortBy(_._1)
      for ((prev, amount) <- sortedKerns) {
        kernPrevChar += prev
        kernAmount += amount
      }
      (next, (offset, sortedKerns.length))
    }).toMap

    val charset = font.glyphs.toSeq.sortBy(_._1)

    // @Serialize(s2ft)
    val buffer = MemoryUtil.memAlloc(64 * 1024 * 1024)

    val Version = 1
    buffer.putMagic("s2ft")
    buffer.putVersion(Version)

    buffer.putIdentifier(imageRes)

    buffer.putInt(charset.length)
    buffer.putInt(kernDataSize)
    buffer.putInt(font.variants.length)

    for ((char, glyph) <- charset) {
      buffer.putInt(char.toInt)
      buffer.putFloat(glyph.advance.toFloat)
      buffer.putFloat(glyph.leftSideBearing.toFloat)
      kerningOffsets.get(char) match {
        case Some((offset, size)) =>
          buffer.putInt(offset)
          buffer.putInt(size)
        case None =>
          buffer.putInt(0)
          buffer.putInt(0)
      }
    }

    for (prev <- kernPrevChar) buffer.putInt(prev.toInt)
    for (amount <- kernAmount) buffer.putFloat(amount.toFloat)

    for (variant <- font.variants) {
      var flags = 0x00
      if (variant.config.signedDistanceField) flags |= 0x01

      buffer.putInt(flags)
      buffer.putInt(variant.height)
      buffer.putFloat(variant.scale.toFloat)
      for ((codepoint, glyph) <- charset) {
        variant.glyphs.get(codepoint) match {
          case Some(rect) =>
            buffer.putInt(rect.channel)

            val rc = rect.rect
            buffer.putShort(rc.x.toShort)
            buffer.putShort(rc.y.toShort)
            buffer.putShort((rc.x + rc.w).toShort)
            buffer.putShort((rc.y + rc.h).toShort)

            buffer.putFloat(rect.offset.x.toFloat)
            buffer.putFloat(rect.offset.y.toFloat)
          case None =>
            buffer.putInt(-1)
            buffer.putShort(0)
            buffer.putShort(0)
            buffer.putShort(0)
            buffer.putShort(0)
            buffer.putFloat(0.0f)
            buffer.putFloat(0.0f)
        }
      }
    }

    buffer.putMagic("E.ft")
    buffer.finish()

    writer.writeFile(file, buffer)

    MemoryUtil.memFree(buffer)
  }
}
