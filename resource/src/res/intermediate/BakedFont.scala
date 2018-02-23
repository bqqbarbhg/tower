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

  class SdfOptions {
    var step: Double = 0
    var edgeValue: Int = 0
    var padding: Int = 0
  }

  class Variant(val height: Int, val config: Config.Res.Font.Variant) {
    var glyphs: Map[Char, GlyphRect] = Map[Char, GlyphRect]()
    var scale: Double = 0.0

    val sdfOptions = if (config.signedDistanceField) {
      val opts = new SdfOptions()

      val radius = height.toDouble / 8.0

      val outlineWidth = height.toDouble * config.maxOutlineRelative
      val outerRadius = outlineWidth + radius
      val innerRadius = radius

      val range = outerRadius + innerRadius
      val step = 256.0 / range
      val edge = outerRadius * step

      opts.edgeValue = edge.toInt
      opts.step = step.toInt
      opts.padding = outerRadius.toInt + 1

      Some(opts)
    } else {
      None
    }
  }

}

class BakedFont extends Resource {

  var image: Image = null
  var variants: Vector[Variant] = Vector[Variant]()
  var kerningPairs: Map[(Char, Char), Double] = Map[(Char, Char), Double]()
  var glyphs: Map[Char, Font.Glyph] = Map[Char, Font.Glyph]()
  var scalePerPixel: Double = 0.0

  def unload(): Unit = {
    image.unload()
    variants = Vector[Variant]()
  }
}
