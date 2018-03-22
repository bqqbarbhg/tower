package menu

import menu.OptionsMenu._
import ui.InputSet.InputArea
import ui.Canvas.TextStyle
import ui._
import asset._
import core._
import ui.SpriteBatch.SpriteDraw


object OptionsMenu {

  val MapModeOptions = Vector(
    "Map",
    "SubData",
    "Persistent",
    "PersistentCoherent",
  )

  class Options {
    var uniformMapMode: String = "SubData"
    var vertexMapMode: String = "Map"
  }

  val MainFont = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")
  private val tLabel = TextStyle(MainFont, 24.0)

  val SmallFrame = Identifier("gui/menu/frame_small.png")
  val Highlight = Identifier("gui/menu/highlight.png")

  def drawNineSlice(canvas: Canvas, layer: Int, sprite: Identifier, layout: Layout, size: Double): Unit = {
    val sd = new SpriteDraw()
    sd.sprite = sprite

    def drawRow(): Unit = {
      sd.m13 = (layout.x0 - size).toFloat
      sd.m11 = size.toFloat * 3.0f
      sd.cropX0 = 0.0f / 3.0f
      sd.cropX1 = 1.0f / 3.0f
      canvas.draw(layer, sd)

      val width = layout.widthPx.toFloat
      sd.m13 = layout.x0.toFloat - width * 1.0f
      sd.m11 = width * 3.0f
      sd.cropX0 = 1.0f / 3.0f
      sd.cropX1 = 2.0f / 3.0f
      canvas.draw(layer, sd)

      sd.m13 = layout.x1.toFloat - size.toFloat * 2.0f
      sd.m11 = size.toFloat * 3.0f
      sd.cropX0 = 2.0f / 3.0f
      sd.cropX1 = 3.0f / 3.0f
      canvas.draw(layer, sd)
    }

    sd.m23 = (layout.y0 - size).toFloat
    sd.m22 = size.toFloat * 3.0f
    sd.cropY0 = 0.0f / 3.0f
    sd.cropY1 = 1.0f / 3.0f
    drawRow()

    val height = layout.heightPx.toFloat
    sd.m23 = layout.y0.toFloat - height * 1.0f
    sd.m22 = height * 3.0f
    sd.cropY0 = 1.0f / 3.0f
    sd.cropY1 = 2.0f / 3.0f
    drawRow()

    sd.m23 = layout.y1.toFloat - size.toFloat * 2.0f
    sd.m22 = size.toFloat * 3.0f
    sd.cropY0 = 2.0f / 3.0f
    sd.cropY1 = 3.0f / 3.0f
    drawRow()
  }

  abstract class Dropdown {
    var layout: Layout = Layout.Empty
    var open: Boolean = false

    val openInput = new InputArea()
    val optionInput = new InputArea()

    def optionNames: Seq[String]
    def get: Int
    def set(value: Int)

    def update(inputs: InputSet, canvas: Canvas): Unit = {
      val opts = optionNames
      inputs.add(openInput, layout)

      drawNineSlice(canvas, 0, SmallFrame, layout, 32.0)
      if (openInput.focused)
        canvas.draw(0, Highlight, layout)

      val selected = optionNames(get)
      canvas.drawText(0, tLabel, layout, selected)

      if (open) {

        val itemHeight = layout.heightUnits
        val box = layout.copy.extendBottom(15.0).edgeBottom.extendBottom(itemHeight * opts.length)

        drawNineSlice(canvas, 1, SmallFrame, box, 32.0)

        for ((opt, index) <- opts.zipWithIndex) {
          val item = box.pushTop(itemHeight)
          inputs.add(optionInput, item, index)
          canvas.drawText(1, tLabel, item, opt)

          if (optionInput.focusIndex == index)
            canvas.draw(1, Highlight, item)
        }
      }

    }
  }

}

class OptionsMenu {

  val options = new Options()

  val uniformMap = new Dropdown() {
    def optionNames = MapModeOptions
    def get = MapModeOptions.indexOf(options.uniformMapMode)
    def set(value: Int) = options.uniformMapMode = MapModeOptions(value)
  }

  val vertexMap = new Dropdown() {
    def optionNames = MapModeOptions
    def get = MapModeOptions.indexOf(options.vertexMapMode)
    def set(value: Int) = options.vertexMapMode = MapModeOptions(value)
  }

  val dropdowns = Vector(uniformMap, vertexMap)

  def update(inputs: InputSet, canvas: Canvas): Unit = {

    if (inputs.clicked) {
      for (dropdown <- dropdowns) {
        dropdown.open = false
      }
    }

    for (dropdown <- dropdowns) {
      if (dropdown.openInput.clicked) {
        dropdown.open = true
      }
      val index = dropdown.optionInput.clickIndex
      if (index >= 0) {
        dropdown.set(index)
      }
    }

    val container = Layout.screen720p.padAround(50.0)
    val buttons = container.pushLeft(200.0)
    uniformMap.layout = buttons.pushTop(20.0)
    buttons.padTop(30.0)
    vertexMap.layout = buttons.pushTop(20.0)

    uniformMap.update(inputs, canvas)
    vertexMap.update(inputs, canvas)
  }

}
