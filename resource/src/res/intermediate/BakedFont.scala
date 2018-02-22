package res.intermediate

import util.Rectangle

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import BakedFont._
import core._

object BakedFont {

  /**
    * Location of a glyph in the bitmap
    *
    * @param offset Offset to apply to the glyph when rendering
    * @param rect Area of the glyph
    * @param channel Channel in the bitmap (0 to 3 -> RGBA)
    */
  case class GlyphRect(offset: Vector2, rect: Rectangle, channel: Int)

  class Variant(val height: Int, val config: Config.Res.Font.Variant) {
    var glyphs: Map[Char, GlyphRect] = Map[Char, GlyphRect]()
    var scale: Double = 0.0
  }

}

class BakedFont extends Resource {

  var image: Image = null
  var variants: Vector[Variant] = Vector[Variant]()
  var kerningPairs: Map[(Char, Char), Double] = Map[(Char, Char), Double]()
  var glyphs: Map[Char, Font.Glyph] = Map[Char, Font.Glyph]()

  def unload(): Unit = {
    image.unload()
    variants = Vector[Variant]()
  }
}
