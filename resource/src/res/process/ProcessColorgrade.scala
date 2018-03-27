package res.process

import res.intermediate._

/**
  * Crop the color-grading lookup from the image and apply potential color-space
  * transformations.
  */
object ProcessColorgrade {

  def processColorgrade(image: Image, config: Config.Res.Colorgrade): Texture = {
    val size = config.resolution
    require(image.width >= size * size, "Image must be large enough to contain color table")
    require(image.height >= size, "Image must be large enough to contain color table")
    val lut = Image.createInt16(size * size, size, false)

    for {
      y <- 0 until size
      x <- 0 until (size * size)
    } {
      val px = image.getPixel(x, y)

      // TODO: Convert from sRGB etc if necessary

      lut.setPixel(x, y, px)
    }

    val tex = CreateTexture.createTexture(lut, config.texture)
    lut.unload()

    tex
  }

}

