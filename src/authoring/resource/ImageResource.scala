package tower.authoring.resource

import tower.util.Color

object Pixel {
  def apply(r: Int, g: Int, b: Int, a: Int): Pixel = new Pixel(
      (r & 0xFF) <<  0 |
      (g & 0xFF) <<  8 |
      (b & 0xFF) << 16 |
      (a & 0xFF) << 24 )
}

/** Represents a 32-bit RGBA pixel. Without any color-space information. */
class Pixel(val bits: Int) extends AnyVal {

  def r: Int = (bits >>>  0) & 0xFF
  def g: Int = (bits >>>  8) & 0xFF
  def b: Int = (bits >>> 16) & 0xFF
  def a: Int = (bits >>> 24) & 0xFF

  def asSrgb: Color = Color.fromSrgb(r, g, b, a)

}

class ImageResource(name: String) extends tower.authoring.Resource(name) {

  var width: Int = 0
  var height: Int = 0
  var data: Array[Int] = Array[Int]()

  def pixel(x: Int, y: Int): Pixel = new Pixel(data(y * width + x))
  def pixels: Seq[Pixel] = (for (y <- 0 until height; x <- 0 until width) yield pixel(x, y)).toSeq

  def setPixel(x: Int, y: Int, color: Pixel): Unit = { data(y * width + x) = color.bits }

}
