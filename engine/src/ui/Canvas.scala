package ui

import javafx.scene.effect.BlendMode

import core._
import render._
import asset._
import asset.{DynamicAsset, FontAsset}
import ui.Canvas._
import ui.Font.TextDraw
import ui.SpriteBatch.SpriteDraw

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Canvas {

  class Shared extends Unloadable{
    val spriteBatch = new SpriteBatch()

    def unload(): Unit = {
      spriteBatch.unload()
    }
  }

  val shared = DynamicAsset("Canvas.Shared", new Shared)

  case class Outline(size: Double, color: Color = Color.Black)
  val NoOutline = Outline(0.0, Color.TransparentBlack)

  case class Align(val x: Double, val y: Double)
  val AlignTopLeft = Align(0.0, 0.0)
  val AlignCenter = Align(0.5, 0.5)
  val AlignTopRight = Align(1.0, 0.0)

  case class TextStyle(font: FontAsset, height: Double = 16.0, color: Color = Color.White, outline: Outline = NoOutline, align: Align = AlignTopLeft) {

    def scaled(amount: Double): TextStyle = this.copy(height = height * amount)

    def measureWidth(text: String): Double = measureWidth(text, 0, text.length, '\0')
    def measureWidth(text: String, nextChar: Char): Double = measureWidth(text, 0, text.length, nextChar)
    def measureWidth(text: String, offset: Int, length: Int): Double = measureWidth(text, offset, length, '\0')
    def measureWidth(text: String, offset: Int, length: Int, nextChar: Char): Double = {
      if (length == 0) return 0.0

      val actualFont = font.get
      var ix = offset
      val end = offset + length
      var pos = 0.0
      if (ix < end - 1) {
        var prevChar = text(ix)
        ix += 1
        while (ix < end) {
          val ch = text(ix)
          pos += actualFont.getAdvance(prevChar, height, ch)
          prevChar = ch
          ix += 1
        }
        pos += actualFont.getAdvance(prevChar, height, nextChar)
      } else if (ix < end) {
        pos = actualFont.getAdvance(text(ix), height, nextChar)
      }
      pos
    }

  }

  private class InternalLayer(val index: Int) {
    var blendMode: Renderer.BlendMode = Renderer.BlendPremultipliedAlpha
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

  def draw(layer: Int, sprite: Identifier, layout: Layout): Unit = {
    draw(layer, sprite, layout.x0, layout.y0, layout.widthPx, layout.heightPx)
  }

  def draw(layer: Int, sprite: Identifier, pos: Vector2, size: Vector2): Unit = {
    draw(layer, sprite, pos.x, pos.y, size.x, size.y)
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

  def draw(layer: Int, sprite: Identifier, layout: Layout, color: Color): Unit = {
    draw(layer, sprite, layout.x0, layout.y0, layout.widthPx, layout.heightPx, color)
  }

  def draw(layer: Int, sprite: Identifier, pos: Vector2, size: Vector2, color: Color): Unit = {
    draw(layer, sprite, pos.x, pos.y, size.x, size.y, color)
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

  def draw(layer: Int, spriteDraw: SpriteDraw): Unit = {
    getInternalLayer(layer).sprites += spriteDraw.copy
  }

  def drawText(layer: Int, style: TextStyle, layout: Layout, text: String): Double =
    drawText(layer, style, layout, text, 0, text.length)

  def drawText(layer: Int, style: TextStyle, layout: Layout, text: String, offset: Int, length: Int): Double = {
    val sizedStyle = style.scaled(layout.unit.y)
    drawText(layer, sizedStyle, Vector2(layout.x0, layout.y0), Vector2(layout.widthPx, layout.heightPx), text, offset, length)
  }

  def drawText(layer: Int, style: TextStyle, x: Double, y: Double, text: String): Double =
    drawText(layer, style, Vector2(x, y), text, 0, text.length)

  def drawText(layer: Int, style: TextStyle, x: Double, y: Double, text: String, offset: Int, length: Int): Double =
    drawText(layer, style, Vector2(x, y), text, offset, length)

  def drawText(layer: Int, style: TextStyle, position: Vector2, text: String): Double =
    drawText(layer, style, position, text, 0, text.length)

  def drawText(layer: Int, style: TextStyle, position: Vector2, text: String, offset: Int, length: Int): Double = {
    drawText(layer, style, position, Vector2.Zero, text, offset, length)
  }

  def drawText(layer: Int, style: TextStyle, position: Vector2, size: Vector2, text: String): Double = {
    drawText(layer, style, position, size, text, 0, text.length)
  }

  def drawText(layer: Int, style: TextStyle, position: Vector2, size: Vector2, text: String, offset: Int, length: Int): Double = {
    val il = getInternalLayer(layer)

    var pos = position

    if (style.align.x != 0.0 && size.x > 0.0) {
      val width = style.measureWidth(text, offset, length)
      val dx = (size.x - width) * style.align.x
      pos = pos.copy(x = pos.x + dx)
    }

    if (style.align.y != 0.0 && size.y > 0.0) {
      val height = style.height
      val dy = (size.y - height) * style.align.y
      pos = pos.copy(y = pos.y + dy)
    }

    val draws = il.drawsForFont.getOrElseUpdate(style.font, ArrayBuffer[TextDraw]())
    draws += TextDraw(text, offset, length, pos, style.height, style.color, 0.0, 1)
    if (style.outline.size > 0.0) {
      draws += TextDraw(text, offset, length, pos, style.height, style.outline.color, style.outline.size, 0)
    }

    position.y + style.height
  }

  def drawTextWrapped(layer: Int, style: TextStyle, layout: Layout, text: String): Double = {
    drawTextWrapped(layer, style, layout, text, 0, text.length)
  }

  def drawTextWrapped(layer: Int, style: TextStyle, layout: Layout, text: String, offset: Int, length: Int): Double = {
    val sizedStyle = style.scaled(layout.unit.y)
    drawTextWrapped(layer, sizedStyle, Vector2(layout.x0, layout.y0), Vector2(layout.widthPx, layout.heightPx), text, offset, length)
  }

  def drawTextWrapped(layer: Int, style: TextStyle, position: Vector2, bounds: Vector2, text: String): Double =
    drawTextWrapped(layer, style, position, bounds, text, 0, text.length)
  def drawTextWrapped(layer: Int, style: TextStyle, position: Vector2, bounds: Vector2, text: String, offset: Int, length: Int): Double = {
    val font = style.font.get
    var pos = position

    val hyphenDist = style.height * 6.0

    val lines = WordWrap.wrapText(style.font.get, style.height, bounds.x, text, offset, length,
      hyphenateThreshold = Some(hyphenDist))

    val endY = bounds.y + position.y
    var y = position.y
    for (line <- lines) {
      if (y + style.height > endY) return y

      y = drawText(layer, style, Vector2(position.x, y), line)
    }
    y
  }

  def setLayerBlend(layer: Int, blendMode: Renderer.BlendMode): Unit = {
    getInternalLayer(layer).blendMode = blendMode
  }

  def render(): Unit = {
    val renderer = Renderer.get

    val sb = Canvas.shared.get.spriteBatch
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
