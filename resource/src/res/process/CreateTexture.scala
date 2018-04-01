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
  def toRgba8(image: Image): Texture = {
    val tex = new Texture(image.width, image.height, Texture.Format.Rgba)

    val data = MemoryUtil.memAlloc(image.width * image.height * 4)
    var y = 0
    while (y < image.height) {
      var x = 0
      while (x < image.width) {
        val pixel = image.getPixel(x, y)
        val base = (y * image.width + x) * 4
        data.putInt(base, image.getPixelSrgbInt(x, y))
        x += 1
      }
      y += 1
    }

    tex.levelData = Array(data)
    tex
  }

  /** Convert an image to an RGBA texture */
  def toRgba16(image: Image): Texture = {
    val tex = new Texture(image.width, image.height, Texture.Format.Rgba16)

    val data = MemoryUtil.memAlloc(image.width * image.height * 8)
    for {
      y <- 0 until image.height
      x <- 0 until image.width
    } {
      val pixel = image.getPixel(x, y)
      val (r, g, b, a) = if (image.srgb) pixel.toSrgb16 else pixel.toLinear16
      val base = (y * image.width + x) * 8
      data.putShort(base + 0, r.toShort)
      data.putShort(base + 2, g.toShort)
      data.putShort(base + 4, b.toShort)
      data.putShort(base + 6, a.toShort)
    }

    tex.levelData = Array(data)
    tex
  }

  /** Convert an image to an RGBA texture */
  def toRgb16(image: Image): Texture = {
    val tex = new Texture(image.width, image.height, Texture.Format.Rgb16)

    val data = MemoryUtil.memAlloc(image.width * image.height * 6)
    for {
      y <- 0 until image.height
      x <- 0 until image.width
    } {
      val pixel = image.getPixel(x, y)
      val (r, g, b, a) = if (image.srgb) pixel.toSrgb16 else pixel.toLinear16
      val base = (y * image.width + x) * 6
      data.putShort(base + 0, r.toShort)
      data.putShort(base + 2, g.toShort)
      data.putShort(base + 4, b.toShort)
    }

    tex.levelData = Array(data)
    tex
  }

  /** Convert an image to an RG texture */
  def toRg8(image: Image): Texture = {
    val tex = new Texture(image.width, image.height, Texture.Format.Rg)

    val data = MemoryUtil.memAlloc(image.width * image.height * 2)
    for {
      y <- 0 until image.height
      x <- 0 until image.width
    } {
      val pixel = image.getPixel(x, y)
      val (r, g, b, a) = if (image.srgb) pixel.toSrgb8 else pixel.toLinear8
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
        val hasAlpha = levels.headOption.exists(_.hasAlpha)

        if (config.compressed) {
          levels.map(level => CompressDxt.compressDxt(level, hasAlpha))
        } else {
          if (config.colorDepth == 8) {
            levels.map(level => toRgba8(level))
          } else if (config.colorDepth == 16) {
            if (hasAlpha) {
              levels.map(level => toRgba16(level))
            } else {
              levels.map(level => toRgb16(level))
            }
          } else {
            throw new RuntimeException(s"Unsupported color depth: ${config.colorDepth}")
          }
        }

      case "normal" =>
        if (config.compressed) {
          levels.map(level => CompressDxt.compressBcAlpha(level, 2))
        } else {
          levels.map(level => toRg8(level))
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

    texture.noDownscale = config.noDownscale

    texture
  }

}
