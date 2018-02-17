package res.process

import res.intermediate._
import collection.mutable.ArrayBuffer

/**
  * Generates a chain smaller versions of a image used for mip-mapping.
  */
object GenerateMipmaps {

  /** Generate a chain of mipmaps for the image */
  def generateMipmaps(source: Image): Array[Image] = {
    val mips = new ArrayBuffer[Image]()

    mips += source

    var width = source.width
    var height = source.height
    var done = false
    var prev = source

    while (width > 1 || height > 1) {
      width = math.max(1, width / 2)
      height = math.max(1, height / 2)

      val mip = source.createCompatible(width, height)

      for {
        y <- 0 until height
        x <- 0 until width
      } {
        val bx = x * 2
        val by = y * 2
        val s0 = prev.getPixelClamp(bx + 0, by + 0)
        val s1 = prev.getPixelClamp(bx + 1, by + 0)
        val s2 = prev.getPixelClamp(bx + 0, by + 1)
        val s3 = prev.getPixelClamp(bx + 1, by + 1)

        val sum = (s0 + s1 + s2 + s3) / 4.0
        mip.setPixel(x, y, sum)
      }

      mips += mip
      prev = mip
    }

    mips.toArray
  }

}
