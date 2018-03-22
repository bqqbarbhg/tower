package menu.gui

import core._
import ui.Layout
import ui.InputSet.InputArea

case class ButtonStyle(height: Double,
                       idleBackgroundSprite: Identifier,
                       focusBackgroundSprite: Identifier,
                       clickRadius: Double,
                       padding: Double,
                      )


abstract class Button(val style: ButtonStyle) extends Element {
  def drawContent(parent: Layout): Layout
  def onClick(): Unit = { }

  val input = new InputArea()

  override def update(parent: Layout): Unit = {
    if (enabled) {
      if (input.clicked) {
        onClick()
      }
    }

    if (visible) {
      val rect = parent.pushTop(style.height)

      if (enabled)
        inputs.add(0, input, rect, style.clickRadius)

      val bg = if (input.focused) style.focusBackgroundSprite else style.idleBackgroundSprite
      canvas.draw(0, bg, rect)

      drawContent(rect.copy.padLeft(style.padding))
    }
  }
}

abstract class LabelButton(style: ButtonStyle, val labelStyle: LabelStyle) extends Button(style) {
  def text: String

  def drawContent(parent: Layout): Layout = {
    val rect = parent.pushTop(labelStyle.height)
    canvas.drawText(0, labelStyle.textStyle, rect, text)
    rect
  }
}

