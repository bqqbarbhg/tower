package res.importer

import java.nio.ByteBuffer

import org.lwjgl.stb.STBTTFontinfo
import org.lwjgl.stb.STBTruetype._
import org.lwjgl.system.MemoryUtil

import res.intermediate._
import core._
import util.BufferUtils._
import res.intermediate.Font._

class StbTruetypeFont(var buffer: ByteBuffer) extends Font {

  var fontInfo = STBTTFontinfo.malloc()
  stbtt_InitFont(fontInfo, buffer, 0)

  override def getGlyph(char: Char): Option[Glyph] = {
    val glyph = stbtt_FindGlyphIndex(fontInfo, char.toInt)
    if (glyph > 0) {
      val advance = Array(0)
      val leftSideBearing = Array(0)
      stbtt_GetGlyphHMetrics(fontInfo, glyph, advance, leftSideBearing)
      val result = new Glyph()
      result.advance = advance(0)
      result.leftSideBearing = leftSideBearing(0)
      Some(result)
    } else {
      None
    }
  }

  override def renderGlyphAa(char: Char, heightInPixels: Int, oversampleX: Int, oversampleY: Int): Bitmap = withStack {
    val glyph = stbtt_FindGlyphIndex(fontInfo, char.toInt)
    assert(glyph > 0)

    val scale = stbtt_ScaleForPixelHeight(fontInfo, heightInPixels.toFloat)
    val scaleX = oversampleX * scale
    val scaleY = oversampleY * scale

    val x0 = Array(0)
    val x1 = Array(0)
    val y0 = Array(0)
    val y1 = Array(0)

    var x: Double = 0.0
    var y: Double = 0.0
    var width: Int = 0
    var height: Int = 0
    var buffer: ByteBuffer = null

    if (oversampleX > 1 || oversampleY > 1) {
      stbtt_GetGlyphBitmapBoxSubpixel(fontInfo, glyph, scaleX, scaleY, 0.0f, 0.0f, x0, y0, x1, y1)

      width = x1(0) - x0(0) + (oversampleX - 1)
      height = y1(0) - y0(0) + (oversampleY - 1)
      buffer = alloca(width * height)

      val asubX = Array(0.0f)
      val asubY = Array(0.0f)
      stbtt_MakeGlyphBitmapSubpixelPrefilter(fontInfo, buffer,
        /* Size      */ width, height,
        /* Stride    */ width,
        /* Scale     */ scaleX, scaleY,
        /* Shift     */ 0.0f, 0.0f,
        /* Prefilter */ oversampleX, oversampleY,
        /* Sub       */ asubX, asubY,
        /* Glyph     */ glyph)

      x = x0(0).toDouble / oversampleX.toDouble + asubX(0)
      y = y0(0).toDouble / oversampleY.toDouble + asubY(0)
    } else {

      stbtt_GetGlyphBitmapBox(fontInfo, glyph, scaleX, scaleY, x0, y0, x1, y1)

      width = x1(0) - x0(0)
      height = y1(0) - y0(0)
      buffer = alloca(width * height)

      stbtt_MakeGlyphBitmap(fontInfo, buffer, width, height, width, scaleX, scaleY, glyph)

      x = x0(0).toDouble
      y = y0(0).toDouble
    }

    val data = new Array[Double](width * height)
    for {
      y <- 0 until height
      x <- 0 until width
    } {
      val value = (buffer.get(y * width + x).toInt & 0xFF).toDouble / 255.0
      data(y * width + x) = value
    }

    Bitmap(x, y, width, height, data)
  }

  override def renderGlyphSdf(char: Char, heightInPixels: Int, step: Double, edgeValue: Int, padding: Int): Bitmap = {
    val glyph = stbtt_FindGlyphIndex(fontInfo, char.toInt)
    assert(glyph > 0)

    val scale = stbtt_ScaleForPixelHeight(fontInfo, heightInPixels.toFloat)

    val awidth = Array(0)
    val aheight = Array(0)
    val axoff = Array(0)
    val ayoff = Array(0)
    val buffer = stbtt_GetGlyphSDF(fontInfo,
      /* Scale         */ scale,
      /* Glyph         */ glyph,
      /* Padding       */ padding,
      /* On-edge value */ clamp(edgeValue, 0, 255).toByte,
      /* Pixel-dist    */ step.toFloat,
      /* Size          */ awidth, aheight,
      /* Offset        */ axoff, ayoff)

    val width = awidth(0)
    val height = aheight(0)

    val data = new Array[Double](width * height)

    if (buffer != null && buffer.hasRemaining) {
      for {
        y <- 0 until height
        x <- 0 until width
      } {
        val value = (buffer.get(y * width + x).toInt & 0xFF).toDouble / 255.0
        data(y * width + x) = value
      }

      stbtt_FreeSDF(buffer)
    }
    Bitmap(axoff(0), ayoff(0), width, height, data)
  }

  override def getKerning(prev: Char, next: Char): Double = {
    val glyphPrev = stbtt_FindGlyphIndex(fontInfo, prev.toInt)
    val glyphNext = stbtt_FindGlyphIndex(fontInfo, next.toInt)
    if (glyphPrev > 0 && glyphNext > 0) {
      stbtt_GetGlyphKernAdvance(fontInfo, glyphPrev, glyphNext).toDouble
    } else {
      0.0
    }
  }

  override def getScaleForHeight(heightInPixels: Double): Double = {
    stbtt_ScaleForPixelHeight(fontInfo, heightInPixels.toFloat).toDouble
  }

  override def unload(): Unit = {
    fontInfo.free()
    MemoryUtil.memFree(buffer)
    fontInfo = null
    buffer = null
  }
}

object StbTruetypeImpoter extends Importer {
  override def importType: ImportFileType = ImportFileFont

  def importAsset(asset: AssetFile): Iterable[Resource] = {
    val size = asset.file.length.toInt
    val buffer = MemoryUtil.memAlloc(size)
    buffer.readFromFile(asset.file)
    buffer.finish()

    val font = new StbTruetypeFont(buffer)
    Some(font)
  }

}
