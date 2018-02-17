package res.process

import java.nio.ByteBuffer

import org.lwjgl.system.MemoryUtil
import res.intermediate._

/**
  * Converts an `Image` into a `Texture`. This process is pretty much just a
  * parent-process which calls further ones for compression (eg. `CompressDxt`)
  * and generating mipmaps (`GenerateMipmaps`)
  */
object CreateTexture {

  /** Convert an image to an RGBA texture */
  def toRgba32(image: Image): Texture = {
    val tex = new Texture(image.width, image.height, Texture.Format.Rgba)

    val data = MemoryUtil.memAlloc(image.width * image.height * 4)
    for {
      y <- 0 until image.height
      x <- 0 until image.width
    } {
      val pixel = image.getPixel(x, y)
      val (r, g, b, a) = if (image.srgb) pixel.toSrgb32 else pixel.toLinear32
      val base = (y * image.width + x) * 4
      data.put(base + 0, r.toByte)
      data.put(base + 1, g.toByte)
      data.put(base + 2, b.toByte)
      data.put(base + 3, a.toByte)
    }

    tex.levelData = Array(data)
    tex
  }

  /** Generate a texture from `image` */
  def createTexture(image: Image, config: Config.Res.Texture): Texture = {

    val levels = if (config.hasMipmaps) {
      GenerateMipmaps.generateMipmaps(image)
    } else {
      Array(image)
    }

    val hasAlpha = levels.exists(level => (for {
      y <- 0 until level.height
      x <- 0 until level.width
    } yield level.getPixel(x, y)).forall(_.a < 1.0))

    val levelTextures = if (config.compressed) {
      levels.map(level => CompressDxt.compressDxt(level, hasAlpha))
    } else {
      levels.map(level => toRgba32(level))
    }

    val texture = new Texture(image.width, image.height, levelTextures.head.format)
    texture.levelData = new Array[ByteBuffer](levels.length)
    for ((levelTex, index) <- levelTextures.zipWithIndex) {
      assert(levelTex.format == texture.format)

      // "Move" the data from `level`
      texture.levelData(index) = levelTex.levelData.head
      levelTex.levelData = Array[ByteBuffer]()
    }

    texture
  }

}
