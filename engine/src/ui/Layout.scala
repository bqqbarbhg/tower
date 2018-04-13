package ui

import core._
import render._

object Layout {

  val Empty = new Layout(Vector2.Zero, 0.0, 0.0, 0.0, 0.0)

  /** Enables debug drawing of layouting */
  var debug: Boolean = false

  def screen720p: Layout = {
    val width = RenderTarget.Backbuffer.width.toDouble
    val height = RenderTarget.Backbuffer.height.toDouble
    val unit = Vector2(width / 1280.0, height / 720.0)
    new Layout(unit, 0.0, 0.0, width, height)
  }

  def screen: Layout = {
    val width = RenderTarget.Backbuffer.width.toDouble
    val height = RenderTarget.Backbuffer.height.toDouble
    val unit = Vector2(1.0, 1.0)
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
  def size: Vector2 = Vector2(x1 - x0, y1 - y0)
  def center: Vector2 = Vector2((x0 + x1) * 0.5, (y0 + y1) * 0.5)
  def widthPx: Double = x1 - x0
  def heightPx: Double = y1 - y0
  def widthUnits: Double = (x1 - x0) / unit.x
  def heightUnits: Double = (y1 - y0) / unit.y
  def widthYUnits: Double = (x1 - x0) / unit.y
  def heightXUnits: Double = (y1 - y0) / unit.x

  def contains(point: Vector2): Boolean = !(point.x < x0 || point.y < y0 || point.x > x1 || point.y > y1)

  def pushLeft(amount: Double): Layout = {
    val dx = amount * unit.x
    val px0 = x0
    x0 += dx
    x1 = math.max(x1, x0)
    Layout.debug(x0, y0, x0, y1)
    new Layout(unit, px0, y0, x0, y1)
  }

  def pushRight(amount: Double): Layout = {
    val dx = amount * unit.x
    val px1 = x1
    x1 -= dx
    x0 = math.min(x0, x1)
    Layout.debug(x1, y0, x1, y1)
    new Layout(unit, x1, y0, px1, y1)
  }

  def pushTop(amount: Double): Layout = {
    val dy = amount * unit.y
    val py0 = y0
    y0 += dy
    y1 = math.max(y1, y0)
    Layout.debug(x0, y0, x1, y0)
    new Layout(unit, x0, py0, x1, y0)
  }

  def pushBottom(amount: Double): Layout = {
    val dy = amount * unit.y
    val py1 = y1
    y1 -= dy
    y0 = math.min(y0, y1)
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

  def contain(targetWidth: Double, targetHeight: Double, anchor: Vector2 = Vector2(0.5, 0.5)): Layout = {
    val targetAspect = targetWidth / targetHeight
    val ownAspect = widthPx / heightPx
    val res = if (targetAspect > ownAspect) {
      val off = (heightPx - widthPx / targetAspect) * anchor.y
      val uu = widthPx / targetWidth
      val u = Vector2(uu, uu)
      new Layout(u, x0, y0 + off, x1, y0 + off + widthPx / targetAspect)
    } else {
      val off = (widthPx - heightPx * targetAspect) * anchor.x
      val uu = heightPx / targetHeight
      val u = Vector2(uu, uu)
      new Layout(u, x0 + off, y0, x0 + off + heightPx * targetAspect, y1)
    }
    Layout.debug(res.x0, res.y0, res.x1, res.y0)
    Layout.debug(res.x0, res.y1, res.x1, res.y1)
    Layout.debug(res.x0, res.y0, res.x0, res.y1)
    Layout.debug(res.x1, res.y0, res.x1, res.y1)
    res
  }

  def containSnapped(targetWidth: Double, targetHeight: Double, snapScale: Double = 1.0, magScale: Double = 1.0, minScale: Double = 1.0, anchor: Vector2 = Vector2(0.5, 0.5), relativeScale: Double = 1.0): Layout = {
    val targetAspect = targetWidth / targetHeight
    val ownAspect = widthPx / heightPx
    var scale = 0.0
    var resWidth = 0.0
    var resHeight = 0.0

    def doSnap(fullScale: Double) = {
      val scale = fullScale * relativeScale

      if (scale < 1.0) {
        val min = math.floor(scale * minScale) / minScale
        if (min > 0.0) min else scale
      } else if (scale > 1.0) {
        math.floor(scale * magScale) / magScale
      } else {
        1.0
      }
    }

    if (targetAspect > ownAspect) {
      scale = doSnap(widthPx / targetWidth)
      resWidth = targetWidth * scale
      resHeight = resWidth / targetAspect
    } else {
      scale = doSnap(heightPx / targetHeight)
      resHeight = targetHeight * scale
      resWidth = resHeight * targetAspect
    }

    val u = Vector2(scale, scale)
    val rx = math.floor((x0 + (widthPx - resWidth) * anchor.x) * snapScale) / snapScale
    val ry = math.floor((y0 + (heightPx - resHeight) * anchor.y) * snapScale) / snapScale
    val res = new Layout(u, rx, ry, rx + resWidth, ry + resHeight)

    Layout.debug(res.x0, res.y0, res.x1, res.y0)
    Layout.debug(res.x0, res.y1, res.x1, res.y1)
    Layout.debug(res.x0, res.y0, res.x0, res.y1)
    Layout.debug(res.x1, res.y0, res.x1, res.y1)
    res
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

  def edgeTop    = new Layout(unit, x0, y0, x1, y0)
  def edgeBottom = new Layout(unit, x0, y1, x1, y1)
  def edgeLeft   = new Layout(unit, x0, y0, x0, y1)
  def edgeRight  = new Layout(unit, x1, y0, x1, y1)

  def extendLeft(amount: Double): Layout = {
    val dx = amount * unit.x
    val px0 = x0
    x0 -= dx
    Layout.debug(x0, y0, px0, y0)
    Layout.debug(x0, y1, px0, y1)
    Layout.debug(x0, y0, x0, y1)
    this
  }

  def extendRight(amount: Double): Layout = {
    val dx = amount * unit.x
    val px1 = x1
    x1 += dx
    Layout.debug(px1, y0, x1, y0)
    Layout.debug(px1, y1, x1, y1)
    Layout.debug(x1, y0, x1, y1)
    this
  }

  def extendTop(amount: Double): Layout = {
    val dy = amount * unit.y
    val py0 = y0
    y0 -= dy
    Layout.debug(x0, y0, x0, py0)
    Layout.debug(x1, y0, x1, py0)
    Layout.debug(x0, y0, x1, y0)
    this
  }

  def extendBottom(amount: Double): Layout = {
    val dy = amount * unit.y
    val py1 = y1
    y1 += dy
    Layout.debug(x0, py1, x0, y1)
    Layout.debug(x1, py1, x1, y1)
    Layout.debug(x0, y1, x1, y1)
    this
  }

  override def toString: String = s"Layout(${x0.toInt}, ${y0.toInt}, ${x1.toInt}, ${y1.toInt})"

  def copy: Layout = new Layout(unit, x0, y0, x1, y1)

}
