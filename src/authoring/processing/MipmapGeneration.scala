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
        val s0 = samplePrev(bx + 0, by + 0)
        val s1 = samplePrev(bx + 1, by + 0)
        val s2 = samplePrev(bx + 0, by + 1)
        val s3 = samplePrev(bx + 1, by + 1)

        val r = math.min((s0.r + s1.r + s2.r + s3.r) / 4, 255)
        val g = math.min((s0.g + s1.g + s2.g + s3.g) / 4, 255)
        val b = math.min((s0.b + s1.b + s2.b + s3.b) / 4, 255)
        val a = math.min((s0.a + s1.a + s2.a + s3.a) / 4, 255)

        mip.setPixel(x, y, Pixel(r, g, b, a))
      }

      mips += mip
      prev = mip
    }

    mips.toArray
  }

}

