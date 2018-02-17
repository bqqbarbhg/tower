package res.process

import core._
import java.nio.ByteBuffer

import com.sun.prism.impl.TextureResourcePool
import res.intermediate._
import org.lwjgl.BufferUtils
import org.lwjgl.stb.STBDXT
import org.lwjgl.system.MemoryUtil

/**
  * Compress textures using DXT aka BC - block compression.
  * Uses stb_dxt
  */
object CompressDxt {

  /**
    * Compress to DXT1 or DXT5 depending on `hasAlpha`
    */
  def compressDxt(image: Image, hasAlpha: Boolean): Texture = withStack {
    val src = alloca(64)

    val numBlocksX = (image.width + 3) / 4
    val numBlocksY = (image.height + 3) / 4

    val blockSize = if (hasAlpha) 16 else 8
    val dstSize = numBlocksX * numBlocksY * blockSize
    val dst = MemoryUtil.memAlloc(dstSize)

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

        val pixel = image.getPixel(xx, yy)
        val (r, g, b, a) = if (image.srgb) pixel.toSrgb32 else pixel.toLinear32
        val base = (dy * 4 + dx) * 4
        src.put(base + 0, r.toByte)
        src.put(base + 1, g.toByte)
        src.put(base + 2, b.toByte)
        src.put(base + 3, a.toByte)
      }

      dst.position((y * numBlocksX + x) * blockSize)
      STBDXT.stb_compress_dxt_block(dst, src, hasAlpha, STBDXT.STB_DXT_HIGHQUAL)
    }

    dst.finish()

    val format = if (hasAlpha) Texture.Format.Dxt5 else Texture.Format.Dxt1
    val tex = new Texture(image.width, image.height, format)
    tex.levelData = Array(dst)
    tex
  }

}

