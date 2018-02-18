package res.process

import java.io.File

import org.lwjgl.stb.STBImageWrite._
import org.lwjgl.system.MemoryUtil
import res.intermediate._

/**
  * Saves an image as a png-file for debugging purposes.
  */
object SaveDebugImage {

  def saveImage(file: File, image: Image): Unit = {
    val buffer = MemoryUtil.memAlloc(image.width * image.height * 4)

    for {
      y <- 0 until image.height
      x <- 0 until image.width
    } {
      val (r, g, b, a) = if (image.srgb) {
        image.getPixel(x, y).toSrgb32
      } else {
        image.getPixel(x, y).toLinear32
      }
      val base = (y * image.width + x) * 4
      buffer.put(base + 0, r.toByte)
      buffer.put(base + 1, g.toByte)
      buffer.put(base + 2, b.toByte)
      buffer.put(base + 3, a.toByte)
    }

    stbi_write_png(file.getAbsolutePath, image.width, image.height, 4, buffer, 0)

    MemoryUtil.memFree(buffer)
  }

  def saveImageChannel(file: File, image: Image, channel: Int): Unit = {
    val buffer = MemoryUtil.memAlloc(image.width * image.height)

    for {
      y <- 0 until image.height
      x <- 0 until image.width
    } {
      val (r, g, b, a) = if (image.srgb) {
        image.getPixel(x, y).toSrgb32
      } else {
        image.getPixel(x, y).toLinear32
      }
      val base = y * image.width + x
      val comp = channel match {
        case 0 => r
        case 1 => g
        case 2 => b
        case 3 => a
      }
      buffer.put(base, comp.toByte)
    }

    stbi_write_png(file.getAbsolutePath, image.width, image.height, 1, buffer, 0)

    MemoryUtil.memFree(buffer)
  }

}
