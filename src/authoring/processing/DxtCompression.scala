package tower.authoring.processing

import org.lwjgl.BufferUtils
import org.lwjgl.stb.STBDXT
import tower.authoring.resource._

object DxtCompression {

  def compressDxt(image: ImageResource): TextureResource = {
    val src = BufferUtils.createByteBuffer(64)

    val hasAlpha = !image.pixels.forall(_.a >= 255)

    val numBlocksX = (image.width + 3) / 4
    val numBlocksY = (image.height + 3) / 4

    val blockSize = if (hasAlpha) 16 else 8
    val dstSize = numBlocksX * numBlocksY * blockSize
    val dst = BufferUtils.createByteBuffer(dstSize)

    for {
      y <- 0 until numBlocksX
      x <- 0 until numBlocksY
    } {
      for {
        dy <- 0 until 4
        dx <- 0 until 4
      } {
        val xx = math.min(x * 4 + dx, image.width - 1)
        val yy = math.min(y * 4 + dy, image.height - 1)
        val pixel = image.pixel(xx, yy)
        val base = (dy * 4 + dx) * 4
        src.put(base + 0, pixel.r.toByte)
        src.put(base + 1, pixel.g.toByte)
        src.put(base + 2, pixel.b.toByte)
        src.put(base + 3, pixel.a.toByte)
      }

      dst.position((y * numBlocksX + x) * blockSize)
      STBDXT.stb_compress_dxt_block(dst, src, hasAlpha, STBDXT.STB_DXT_HIGHQUAL)
    }

    dst.position(0)
    val tex = new TextureResource(image.name)
    tex.width = image.width
    tex.height = image.height
    tex.data = dst
    tex.format = if (hasAlpha) {
      TextureResource.Format.Dxt5
    } else {
      TextureResource.Format.Dxt1
    }
    tex
  }

}
