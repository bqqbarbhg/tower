package res.process

import java.nio.ByteBuffer

import core.Color
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

  /** Convert an image to an RG texture */
  def toRg32(image: Image): Texture = {
    val tex = new Texture(image.width, image.height, Texture.Format.Rg)

    val data = MemoryUtil.memAlloc(image.width * image.height * 2)
    for {
      y <- 0 until image.height
      x <- 0 until image.width
    } {
      val pixel = image.getPixel(x, y)
      val (r, g, b, a) = if (image.srgb) pixel.toSrgb32 else pixel.toLinear32
      val base = (y * image.width + x) * 2
      data.put(base + 0, r.toByte)
      data.put(base + 1, g.toByte)
    }

    tex.levelData = Array(data)
    tex
  }

  /** Generate a texture from `image` */
  def createTexture(image: Image, config: Config.Res.Texture): Texture = {

    var imageToFree: Option[Image] = None

    val imgData = if (config.premultiplyAlpha) {
      val premultiplied = PremultiplyAlpha.premultiplyAlpha(image)
      imageToFree = Some(premultiplied)
      premultiplied
    } else
      image

    val levels = if (config.hasMipmaps) {
      GenerateMipmaps.generateMipmaps(imgData)
    } else {
      Array(imgData)
    }

    val levelTextures = config.semantic match {
      case "color" =>
        val hasAlpha = levels.exists(level => (for {
          y <- 0 until level.height
          x <- 0 until level.width
        } yield level.getPixel(x, y)).forall(_.a < 1.0))

        if (config.compressed) {
          levels.map(level => CompressDxt.compressDxt(level, hasAlpha))
        } else {
          levels.map(level => toRgba32(level))
        }

      case "normal" =>
        if (config.compressed) {
          levels.map(level => CompressDxt.compressBcAlpha(level, 2))
        } else {
          levels.map(level => toRg32(level))
        }

      case other =>
        throw new RuntimeException(s"Unknown texture semantic: $other")
    }

    val readAsLinear = image.srgb && config.readAsLinear

    val texture = new Texture(image.width, image.height, levelTextures.head.format, Some(readAsLinear))
    texture.levelData = new Array[ByteBuffer](levels.length)
    for ((levelTex, index) <- levelTextures.zipWithIndex) {
      assert(levelTex.format == texture.format)

      // "Move" the data from `level`
      texture.levelData(index) = levelTex.levelData.head
      levelTex.levelData = Array[ByteBuffer]()
    }

    for (free <- imageToFree)
      free.unload()

    texture
  }

}
