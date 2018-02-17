package res.intermediate

import java.nio.ByteBuffer

import core._
import org.lwjgl.system.{MemoryManage, MemoryUtil}

object Image {

  def create32(width: Int, height: Int, srgb: Boolean, data: ByteBuffer, pitch: Int = 0): Image = {
    val buf = MemoryUtil.memCalloc(width * height * 4)
    if (data != null) {
      val realPitch = if (pitch > 0) pitch else width * 4

      for {
        y <- 0 until height
        x <- 0 until width
      } {
        val dst = (y * width + x) * 4
        val src = y * realPitch + x * 4
        buf.putInt(dst, data.getInt(src))
      }
    }
    new Image32(width, height, buf, srgb)
  }

  def create32(width: Int, height: Int, srgb: Boolean): Image = create32(width, height, srgb, null)

  /**
    * 32-bit per pixel RGBA image
    * @param data Pointer to data allocated with `MemoryUtil.memAlloc`
    * @param srgb Is the data stored in an sRGB format?
    */
  private class Image32(width: Int, height: Int, data: ByteBuffer, srgb: Boolean) extends Image(width, height, srgb) {

    def getPixel(x: Int, y: Int): Color = {
      val base = (y * width + x) * 4
      val r = data.get(base + 0).toInt & 0xFF
      val g = data.get(base + 1).toInt & 0xFF
      val b = data.get(base + 2).toInt & 0xFF
      val a = data.get(base + 3).toInt & 0xFF
      if (srgb) {
        Color.fromSrgb(r, g, b, a)
      } else {
        Color.fromLinear(r, g, b, a)
      }
    }

    def setPixel(x: Int, y: Int, color: Color): Unit = {
      val (r, g, b, a) = if (srgb) {
        color.toSrgb32
      } else {
        color.toLinear32
      }

      val base = (y * width + x) * 4
      data.put(base + 0, r.toByte)
      data.put(base + 1, g.toByte)
      data.put(base + 2, b.toByte)
      data.put(base + 3, a.toByte)
    }

    def unload(): Unit = {
      MemoryUtil.memFree(data)
    }

    override def createCompatible(width: Int, height: Int): Image = Image.create32(width, height, this.srgb)
  }

}

/** A 2D bitmap image */
abstract class Image(val width: Int, val height: Int, val srgb: Boolean) extends Resource {

  def getPixel(x: Int, y: Int): Color
  def setPixel(x: Int, y: Int, color: Color): Unit

  /** Like `getPixel()` but clamped to image area */
  def getPixelClamp(x: Int, y: Int): Color = {
    val xc = clamp(x, 0, width - 1)
    val yc = clamp(y, 0, height - 1)
    getPixel(xc, yc)
  }

  /** Create a new image of the same format of this image */
  def createCompatible(width: Int, height: Int): Image

}
