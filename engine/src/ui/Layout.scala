package ui

import core._
import render._

object Layout {

  /** Enables debug drawing of layouting */
  var debug: Boolean = true

  def screen720p: Layout = {
    val width = RenderTarget.Backbuffer.width.toDouble
    val height = RenderTarget.Backbuffer.height.toDouble
    val unit = Vector2(width / 1280.0, height / 720.0)
    new Layout(unit, 0.0, 0.0, width, height)
  }

  def debug(x0: Double, y0: Double, x1: Double, y1: Double): Unit = {
    if (debug) {
      LayoutDebugger.addLine(x0, y0, x1, y1)
    }
  }

}

class Layout(var unit: Vector2, var x0: Double, var y0: Double, var x1: Double, var y1: Double) {

  def position: Vector2 = Vector2(x0, y0)
  def size: Vector2 = Vector2(x1, y1)
  def width: Double = x1 - x0
  def height: Double = y1 - y0

  def pushLeft(amount: Double): Layout = {
    val dx = amount * unit.x
    val px0 = x0
    x0 += dx
    Layout.debug(x0, y0, x0, y1)
    new Layout(unit, px0, y0, x0, y1)
  }

  def pushRight(amount: Double): Layout = {
    val dx = amount * unit.x
    val px1 = x1
    x1 -= dx
    Layout.debug(x1, y0, x1, y1)
    new Layout(unit, x1, y0, px1, y1)
  }

  def pushTop(amount: Double): Layout = {
    val dy = amount * unit.y
    val py0 = y0
    y0 += dy
    Layout.debug(x0, y0, x1, y0)
    new Layout(unit, x0, py0, x1, y0)
  }

  def pushBottom(amount: Double): Layout = {
    val dy = amount * unit.y
    val py1 = y1
    y1 -= dy
    Layout.debug(x0, y1, x1, y1)
    new Layout(unit, x0, y1, x1, py1)
  }

  def padLeft(amount: Double): Layout = {
    val dx = amount * unit.x
    x0 += dx
    Layout.debug(x0, y0, x0, y1)
    this
  }

  def padRight(amount: Double): Layout = {
    val dx = amount * unit.x
    x1 -= dx
    Layout.debug(x1, y0, x1, y1)
    this
  }

  def padTop(amount: Double): Layout = {
    val dy = amount * unit.y
    y0 += dy
    Layout.debug(x0, y0, x1, y0)
    this
  }

  def padBottom(amount: Double): Layout = {
    val dy = amount * unit.y
    y1 -= dy
    Layout.debug(x0, y1, x1, y1)
    this
  }

  def padAround(amount: Double): Layout = {
    val dx = amount * unit.x
    val dy = amount * unit.y
    x0 += dx
    y0 += dy
    x1 -= dx
    y1 -= dy
    Layout.debug(x0, y0, x1, y0)
    Layout.debug(x0, y1, x1, y1)
    Layout.debug(x0, y0, x0, y1)
    Layout.debug(x1, y0, x1, y1)
    this
  }

  def pushBottomLeft(amount: Double): Layout = pushBottomLeft(amount, amount)
  def pushBottomLeft(amountX: Double, amountY: Double): Layout = pushBottom(amountX).pushLeft(amountY)
  def pushBottomRight(amount: Double): Layout = pushBottomRight(amount, amount)
  def pushBottomRight(amountX: Double, amountY: Double): Layout = pushBottom(amountX).pushRight(amountY)
  def pushTopLeft(amount: Double): Layout = pushTopLeft(amount, amount)
  def pushTopLeft(amountX: Double, amountY: Double): Layout = pushTop(amountX).pushLeft(amountY)
  def pushTopRight(amount: Double): Layout = pushTopRight(amount, amount)
  def pushTopRight(amountX: Double, amountY: Double): Layout = pushTop(amountX).pushRight(amountY)

  def padBottomLeft(amount: Double): Layout = padBottomLeft(amount, amount)
  def padBottomLeft(amountX: Double, amountY: Double): Layout = padBottom(amountX).padLeft(amountY)
  def padBottomRight(amount: Double): Layout = padBottomRight(amount, amount)
  def padBottomRight(amountX: Double, amountY: Double): Layout = padBottom(amountX).padRight(amountY)
  def padTopLeft(amount: Double): Layout = padTopLeft(amount, amount)
  def padTopLeft(amountX: Double, amountY: Double): Layout = padTop(amountX).padLeft(amountY)
  def padTopRight(amount: Double): Layout = padTopRight(amount, amount)
  def padTopRight(amountX: Double, amountY: Double): Layout = padTop(amountX).padRight(amountY)

  override def toString: String = s"Layout(${x0.toInt}, ${y0.toInt}, ${x1.toInt}, ${y1.toInt})"

  def copy: Layout = new Layout(unit, x0, y0, x1, y1)

}
