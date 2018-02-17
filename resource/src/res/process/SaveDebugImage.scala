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
      val (r, g, b, a) = image.getPixel(x, y).toSrgb32
      val base = (y * image.width + x) * 4
      buffer.put(base + 0, r.toByte)
      buffer.put(base + 1, g.toByte)
      buffer.put(base + 2, b.toByte)
      buffer.put(base + 3, a.toByte)
    }

    stbi_write_png(file.getAbsolutePath, image.width, image.height, 4, buffer, 0)

    MemoryUtil.memFree(buffer)
  }

}
