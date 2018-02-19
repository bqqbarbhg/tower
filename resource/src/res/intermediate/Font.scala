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

  /** Render a glyph into a signed distance field */
  def renderGlyphSdf(char: Char, heightInPixels: Int): Bitmap

  /** Amount of kerning to add to the advance from `prev` to `next` */
  def getKerning(prev: Char, next: Char): Double

}
