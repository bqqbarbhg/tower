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
      Some(new Glyph)
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

    var width: Int = 0
    var height: Int = 0
    var buffer: ByteBuffer = null
    var subX: Double = 0.0
    var subY: Double = 0.0

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

      subX = asubX(0)
      subY = asubY(0)
    } else {

      stbtt_GetGlyphBitmapBox(fontInfo, glyph, scaleX, scaleY, x0, y0, x1, y1)

      width = x1(0) - x0(0)
      height = y1(0) - y0(0)
      buffer = alloca(width * height)

      stbtt_MakeGlyphBitmap(fontInfo, buffer, width, height, width, scaleX, scaleY, glyph)
    }

    val data = new Array[Double](width * height)
    for {
      y <- 0 until height
      x <- 0 until width
    } {
      val value = (buffer.get(y * width + x).toInt & 0xFF).toDouble / 255.0
      data(y * width + x) = value
    }

    Bitmap(width, height, data)
  }

  override def renderGlyphSdf(char: Char, heightInPixels: Int): Bitmap = {
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
      /* Padding       */ 4,
      /* On-edge value */ 128.toByte,
      /* Pixel-dist    */ 64.0f / 4.0f,
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
    Bitmap(width, height, data)
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
