package res.intermediate

import java.nio.{ByteBuffer, ShortBuffer}

import core._
import org.lwjgl.system.{MemoryManage, MemoryUtil}

object Image {

  def createInt8(width: Int, height: Int, srgb: Boolean, data: ByteBuffer, pitch: Int = 0): Image = {
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
    new ImageInt8(width, height, buf, srgb)
  }

  def createInt8(width: Int, height: Int, srgb: Boolean): Image = createInt8(width, height, srgb, null)

  def createInt16(width: Int, height: Int, srgb: Boolean, data: ShortBuffer, pitch: Int = 0): Image = {
    val buf = MemoryUtil.memCalloc(width * height * 8)
    if (data != null) {
      val realPitch = if (pitch > 0) pitch else width * 4

      for {
        y <- 0 until height
        x <- 0 until width
      } {
        val dst = (y * width + x) * 8
        val src = y * realPitch + x * 4
        buf.putShort(dst + 0, data.get(src + 0))
        buf.putShort(dst + 2, data.get(src + 1))
        buf.putShort(dst + 4, data.get(src + 2))
        buf.putShort(dst + 6, data.get(src + 3))
      }
    }
    new ImageInt16(width, height, buf, srgb)
  }

  def createInt16(width: Int, height: Int, srgb: Boolean): Image = createInt8(width, height, srgb, null)

  /**
    * 32-bit per pixel RGBA image
    * @param data Pointer to data allocated with `MemoryUtil.memAlloc`
    * @param srgb Is the data stored in an sRGB format?
    */
  private class ImageInt8(width: Int, height: Int, data: ByteBuffer, srgb: Boolean) extends Image(width, height, srgb) {

    def getPixel(x: Int, y: Int): Color = {
      val base = (y * width + x) * 4
      val r = data.get(base + 0).toInt & 0xFF
      val g = data.get(base + 1).toInt & 0xFF
      val b = data.get(base + 2).toInt & 0xFF
      val a = data.get(base + 3).toInt & 0xFF
      if (srgb) {
        Color.fromSrgb8(r, g, b, a)
      } else {
        Color.fromLinear8(r, g, b, a)
      }
    }

    def setPixel(x: Int, y: Int, color: Color): Unit = {
      val (r, g, b, a) = if (srgb) {
        color.toSrgb8
      } else {
        color.toLinear8
      }

      val base = (y * width + x) * 4
      data.put(base + 0, r.toByte)
      data.put(base + 1, g.toByte)
      data.put(base + 2, b.toByte)
      data.put(base + 3, a.toByte)
    }

    override def getPixelSrgbInt(x: Int, y: Int): Int = {
      val base = (y * width + x) * 4
      data.getInt(base)
    }

    override def hasAlpha: Boolean = {
      var ix = 0
      val length = width * height * 4
      while (ix < length) {
        if ((data.get(ix + 3).toInt & 0xFF) != 0xFF) return true
        ix += 4
      }
      false
    }

    def unload(): Unit = {
      MemoryUtil.memFree(data)
    }

    override def createCompatible(width: Int, height: Int): Image = Image.createInt8(width, height, this.srgb)
  }

  /**
    * 64-bit per pixel RGBA image
    * @param data Pointer to data allocated with `MemoryUtil.memAlloc`
    * @param srgb Is the data stored in an sRGB format?
    */
  private class ImageInt16(width: Int, height: Int, data: ByteBuffer, srgb: Boolean) extends Image(width, height, srgb) {

    def getPixel(x: Int, y: Int): Color = {
      val base = (y * width + x) * 8
      val r = (data.getShort(base + 0).toInt & 0xFFFF) / 65535.0
      val g = (data.getShort(base + 2).toInt & 0xFFFF) / 65535.0
      val b = (data.getShort(base + 4).toInt & 0xFFFF) / 65535.0
      val a = (data.getShort(base + 8).toInt & 0xFFFF) / 65535.0

      if (srgb) {
        Color.fromSrgb(r, g, b, a)
      } else {
        Color.fromLinear(r, g, b, a)
      }
    }

    def setPixel(x: Int, y: Int, color: Color): Unit = {
      val (r, g, b, a) = if (srgb) {
        color.toSrgb16
      } else {
        color.toLinear16
      }

      val base = (y * width + x) * 8
      data.putShort(base + 0, r.toShort)
      data.putShort(base + 2, g.toShort)
      data.putShort(base + 4, b.toShort)
      data.putShort(base + 8, a.toShort)
    }

    def unload(): Unit = {
      MemoryUtil.memFree(data)
    }

    override def createCompatible(width: Int, height: Int): Image = Image.createInt16(width, height, this.srgb)
  }

}

/** A 2D bitmap image */
abstract class Image(val width: Int, val height: Int, val srgb: Boolean) extends Resource {

  def getPixel(x: Int, y: Int): Color
  def setPixel(x: Int, y: Int, color: Color): Unit

  def getPixelSrgbInt(x: Int, y: Int): Int = {
    val pixel = getPixel(x, y)
    if (srgb) pixel.toSrgbInt8 else pixel.toLinearInt8
  }

  def hasAlpha: Boolean = (for {
    y <- 0 until height
    x <- 0 until width
  } yield getPixel(x, y)).forall(_.a < 1.0)

  /** Like `getPixel()` but clamped to image area */
  def getPixelClamp(x: Int, y: Int): Color = {
    val xc = clamp(x, 0, width - 1)
    val yc = clamp(y, 0, height - 1)
    getPixel(xc, yc)
  }

  /** Create a new image of the same format of this image */
  def createCompatible(width: Int, height: Int): Image

}
