package tower.authoring.processing

import org.lwjgl.BufferUtils
import tower.authoring.resource._

import scala.collection.mutable.ArrayBuffer

object MipmapGeneration {

  def generateMipmaps(source: ImageResource): Array[ImageResource] = {
    val mips = new ArrayBuffer[ImageResource]()

    mips += source

    var width = source.width
    var height = source.height
    var done = false
    var prev = source

    def samplePrev(x: Int, y: Int): Pixel = {
      val xx = math.min(x, prev.width - 1)
      val yy = math.min(y, prev.height - 1)
      prev.pixel(xx, yy)
    }

    while (width > 1 || height > 1) {
      width = math.max(1, width / 2)
      height = math.max(1, height / 2)
      val mip = new ImageResource(source.name)
      mip.width = width
      mip.height = height
      mip.data = new Array[Int](width * height)

      for {
        y <- 0 until width
        x <- 0 until height
      } {
        val bx = x * 2
        val by = y * 2
        val s0 = samplePrev(bx + 0, by + 0).asSrgb
        val s1 = samplePrev(bx + 1, by + 0).asSrgb
        val s2 = samplePrev(bx + 0, by + 1).asSrgb
        val s3 = samplePrev(bx + 1, by + 1).asSrgb

        val sum = (s0 + s1 + s2 + s3) / 4.0
        val (r, g, b, a) = sum.toSrgb

        mip.setPixel(x, y, Pixel(r, g, b, a))
      }

      mips += mip
      prev = mip
    }

    mips.toArray
  }

}

