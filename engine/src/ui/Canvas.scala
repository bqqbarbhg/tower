package ui

import core._
import render._
import asset.FontAsset
import ui.Canvas._
import ui.Font.TextDraw
import ui.SpriteBatch.SpriteDraw

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Canvas {

  lazy private val sharedSpriteBatch = new SpriteBatch()

  case class Outline(size: Double, color: Color = Color.Black)
  val NoOutline = Outline(0.0, Color.TransparentBlack)

  case class TextStyle(font: FontAsset, height: Double, color: Color = Color.White, outline: Outline = NoOutline)

  private class InternalLayer(val index: Int) {
    val drawsForFont = mutable.HashMap[FontAsset, ArrayBuffer[TextDraw]]()
    val sprites = ArrayBuffer[SpriteDraw]()
  }
}

class Canvas {

  private var layers = Array[InternalLayer]()

  private def getInternalLayer(layerIndex: Int): InternalLayer = {
    layers.find(_.index == layerIndex).getOrElse {
      val layer = new InternalLayer(layerIndex)
      layers = (layers :+ layer).sortBy(_.index)
      layer
    }
  }

  def drawText(layer: Int, style: TextStyle, x: Double, y: Double, text: String): Double =
    drawText(layer, style, Vector2(x, y), text, 0, text.length)

  def drawText(layer: Int, style: TextStyle, x: Double, y: Double, text: String, offset: Int, length: Int): Double =
    drawText(layer, style, Vector2(x, y), text, offset, length)

  def drawText(layer: Int, style: TextStyle, position: Vector2, text: String): Double =
    drawText(layer, style, position, text, 0, text.length)

  def drawText(layer: Int, style: TextStyle, position: Vector2, text: String, offset: Int, length: Int): Double = {
    val il = getInternalLayer(layer)

    val draws = il.drawsForFont.getOrElseUpdate(style.font, ArrayBuffer[TextDraw]())
    draws += TextDraw(text, offset, length, position, style.height, style.color, 0.0, 1)
    if (style.outline.size > 0.0) {
      draws += TextDraw(text, offset, length, position, style.height, style.outline.color, style.outline.size, 0)
    }

    position.y + style.height
  }

  def drawTextWrapped(layer: Int, style: TextStyle, position: Vector2, bounds: Vector2, text: String): Double =
    drawTextWrapped(layer, style, position, bounds, text, 0, text.length)
  def drawTextWrapped(layer: Int, style: TextStyle, position: Vector2, bounds: Vector2, text: String, offset: Int, length: Int): Double = {
    val font = style.font.get
    var pos = position

    val lines = WordWrap.wrapText(style.font.get, style.height, bounds.x, text, offset, length,
      hyphenateThreshold = Some(100.0))

    var y = position.y
    for (line <- lines) {
      if (y + style.height > bounds.y) return y

      y = drawText(layer, style, Vector2(position.x, y), line)
    }
    y
  }

  def render(): Unit = {
    val renderer = Renderer.get
    renderer.setBlend(true)

    val sb = Canvas.sharedSpriteBatch
    var sbNeedsFlush = false

    for (layer <- layers) {
      for (sprite <- layer.sprites) {
        sb.draw(sprite)
      }

      if (layer.sprites.nonEmpty)
        sbNeedsFlush = true

      if (layer.drawsForFont.nonEmpty && sbNeedsFlush) {
        sb.flush()
        sbNeedsFlush = false
      }

      for ((font, texts) <- layer.drawsForFont) {
        font.get.render(texts)
      }

      layer.sprites.clear()
      layer.drawsForFont.clear()
    }

    if (sbNeedsFlush)
      sb.flush()

  }

}