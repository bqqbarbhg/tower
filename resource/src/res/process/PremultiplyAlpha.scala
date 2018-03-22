package res.process

import res.intermediate._

/** Multiply the color channel with the alpha channel for better filtering */
object PremultiplyAlpha {

  def premultiplyAlpha(image: Image): Image = {
    val copy = image.createCompatible(image.width, image.height)
    for {
      y <- 0 until image.height
      x <- 0 until image.width
    } {
      val p = image.getPixel(x, y)
      val pc = p.copy(r = p.r * p.a, g = p.g * p.a, b = p.b * p.a)
      copy.setPixel(x, y, pc)
    }
    copy
  }

}

