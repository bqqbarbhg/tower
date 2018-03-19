package ui

import javafx.scene.effect.BlendMode

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
    var blendMode: Renderer.BlendMode = Renderer.BlendAlpha
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

  def draw(layer: Int, sprite: Identifier, x: Double, y: Double, w: Double, h: Double): Unit = {
    val sd = new SpriteDraw()
    sd.sprite = sprite
    sd.color = Color.White
    sd.m13 = x.toFloat
    sd.m23 = y.toFloat
    sd.m11 = w.toFloat
    sd.m22 = h.toFloat
    getInternalLayer(layer).sprites += sd
  }

  def draw(layer: Int, sprite: Identifier, x: Double, y: Double, w: Double, h: Double, color: Color): Unit = {
    val sd = new SpriteDraw()
    sd.sprite = sprite
    sd.color = color
    sd.m13 = x.toFloat
    sd.m23 = y.toFloat
    sd.m11 = w.toFloat
    sd.m22 = h.toFloat
    getInternalLayer(layer).sprites += sd
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

  def setLayerBlend(layer: Int, blendMode: Renderer.BlendMode): Unit = {
    getInternalLayer(layer).blendMode = blendMode
  }

  def render(): Unit = {
    val renderer = Renderer.get

    val sb = Canvas.sharedSpriteBatch
    var sbNeedsFlush = false

    for (layer <- layers) {
      if (layer.sprites.nonEmpty) {
        renderer.setBlend(layer.blendMode)
        sbNeedsFlush = true
      }

      for (sprite <- layer.sprites) {
        sb.draw(sprite)
      }

      if (layer.drawsForFont.nonEmpty) {
        if (sbNeedsFlush) {
          sb.flush()
          sbNeedsFlush = false
        }

        renderer.setBlend(Renderer.BlendAlpha)
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
