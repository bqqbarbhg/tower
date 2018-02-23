package res.intermediate

import Font._

object Font {

  class Glyph {
    var advance: Double = 0.0
    var leftSideBearing: Double = 0.0
  }

  case class Bitmap(x: Double, y: Double, width: Int, height: Int, data: Array[Double]) {
    def getPixel(x: Int, y: Int): Double = data(y * width + x)
  }

}

/**
  * There is no generic internal representation of a vector font-file so the
  * actual `Font` implementations are closely tied to their importers.
  */
abstract class Font extends Resource {

  /** Retrieve a glyph info (if available) */
  def getGlyph(char: Char): Option[Glyph]

  /** Render a glyph into a bitmap using antialias */
  def renderGlyphAa(char: Char, heightInPixels: Int, oversampleX: Int = 1, oversampleY: Int = 1): Bitmap

  /** Render a signed distance field of the glyph into a bitmap.
    *
    * @param char Character to render
    * @param heightInPixels Size of the character to render
    * @param step How many value-units to advance per pixel
    * @param edgeValue Value at the edge of the character (0-255)
    * @param padding Amount of pixels to add to each side outside of the character's bounding box
    */
  def renderGlyphSdf(char: Char, heightInPixels: Int, distancePerPixel: Double, edgeValue: Int, padding: Int): Bitmap

  /** Amount of kerning to add to the advance from `prev` to `next` */
  def getKerning(prev: Char, next: Char): Double

  /** Get factor to scale the font measures for pixel height */
  def getScaleForHeight(heightInPixels: Double): Double

}
