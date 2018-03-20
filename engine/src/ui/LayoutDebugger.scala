package ui

import asset.FontAsset
import core._
import render._

import collection.mutable.ArrayBuffer
import platform.AppWindow
import ui.Canvas._

object LayoutDebugger {
  case class DebugLine(x0: Double, y0: Double, x1: Double, y1: Double, stackTrace: Array[StackTraceElement])

  val debugLines = ArrayBuffer[DebugLine]()

  def addLine(x0: Double, y0: Double, x1: Double, y1: Double): Unit = {
    val stack = Thread.currentThread.getStackTrace
    debugLines += DebugLine(x0, y0, x1, y1, stack)
  }

  val DefaultColor = Color.rgb(0xFF0000)
  val SelectedColor = Color.rgb(0xFF00FF)

  val BannedStackFiles = Set("Iterable.scala", "IterableLike.scala", "Iterator.scala")
  val RootCandidates = Set(("GameState.scala", "update"))

  val Font = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")
  val tStackTrace = TextStyle(Font, 18, outline = Outline(1.0))
  val lMain = 0
  val canvas = new Canvas()

  def render(): Unit = {
    val mouse = AppWindow.mousePosition

    val lineDist = debugLines.flatMap(line => {
      if (mouse.x >= line.x0 && mouse.x <= line.x1) {
        if (mouse.y >= line.y0 && mouse.y <= line.y1) {
          Some((line, 0.0))
        } else {
          Some((line, math.abs(line.y0 - mouse.y)))
        }
      }
      else if (mouse.y >= line.y0 && mouse.y <= line.y1) {
        Some((line, math.abs(line.x0 - mouse.x)))
      } else {
        None
      }
    })

    var selectedLine: Option[DebugLine] = None

    if (lineDist.nonEmpty) {
      val best = lineDist.minBy(_._2)
      if (best._2 < 10.0) {
        selectedLine = Some(best._1)
      }
    }

    for (line <- debugLines) {
      val isSelected = selectedLine.exists(_ eq line)
      val color = if (isSelected) SelectedColor else DefaultColor
      DebugDraw.drawLine2D(Vector2(line.x0, line.y0), Vector2(line.x1, line.y1), color)
    }

    DebugDraw.render2D()
    val renderer = Renderer.get
    val screenWidth = renderer.currentRenderTarget.width
    val screenHeight = renderer.currentRenderTarget.height

    for (sel <- selectedLine) {
      var xx = mouse.x
      var yy = mouse.y + 30.0

      var goodFrames = sel.stackTrace.drop(3)
        .filterNot(_.getMethodName.contains("$"))
        .filterNot(f => BannedStackFiles.contains(f.getFileName))

      val rootIndex = goodFrames.indexWhere(f => RootCandidates.contains(f.getFileName, f.getMethodName))
      if (rootIndex >= 0) {
        goodFrames = goodFrames.take(rootIndex)
      }

      val frameTexts = goodFrames.map(frame => {
        s"${frame.getFileName}:${frame.getLineNumber}: ${frame.getMethodName}"
      })

      if (frameTexts.length > 0) {
        val height = frameTexts.length * tStackTrace.height
        val width = frameTexts.map(tStackTrace.measureWidth).max + 10.0

        xx = math.min(xx, screenWidth - width)
        if (yy + height > screenHeight) {
          yy -= height + 35.0
        }

        for (text <- frameTexts) {
          yy = canvas.drawText(lMain, tStackTrace, xx, yy, text)
        }
      }
    }

    canvas.render()

    debugLines.clear()
  }
}

