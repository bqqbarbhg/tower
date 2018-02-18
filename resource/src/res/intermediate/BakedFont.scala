package res.intermediate

import util.Rectangle

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import BakedFont._

object BakedFont {

  /**
    * Location of a glyph in the bitmap
    *
    * @param rect Area of the glyph
    * @param channel Channel in the bitmap (0 to 3 -> RGBA)
    */
  case class GlyphRect(rect: Rectangle, channel: Int)

  class Variant(val height: Int, val config: Config.Res.Font.Variant) {
    var glyphs: Map[Char, GlyphRect] = Map[Char, GlyphRect]()
  }

}

class BakedFont extends Resource {

  var image: Image = null
  var variants: Vector[Variant] = Vector[Variant]()

  def unload(): Unit = {
    image.unload()
    variants = Vector[Variant]()
  }
}
