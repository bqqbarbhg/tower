package menu.gui

import core._
import ui._
import ui.Canvas.TextStyle
import ui.InputSet.InputArea
import TextBox._
import platform.{AppWindow, KeyEvent}
import locale.LocaleString._

case class ButtonInputStyle(height: Double,
                            textStyle: TextStyle,
                            idleBackgroundSprite: Identifier,
                            focusBackgroundSprite: Identifier,
                            inputWidth: Double,
                           )

abstract class ButtonInput(val style: ButtonInputStyle, val labelStyle: LabelStyle) extends Element {
  def title: String
  def getBind: String
  def setBind(newBind: String): Unit
  def bindToText(bind: String): String

  var isActive: Boolean = false

  val input = new InputArea()

  override def update(parent: Layout): Unit = {
    if (enabled) {
      if (input.clicked) {
        isActive = !isActive
      } else if (inputs.clicked) {
        isActive = false
      }

      if (isActive) {
        for (key <- AppWindow.keyEvents) {
          if (key.key == KeyEvent.Escape) {
            isActive = false
          } else if (isActive) {
            for (name <- KeyEvent.KeyToName.get(key.key)) {
              setBind(name)
            }
            isActive = false
          }
        }
      }

    } else {
      isActive = false
    }

    if (visible) {
      val fullRect = parent.pushTop(style.height)
      val labelRect = fullRect.copy
      val inputRect = labelRect.pushRight(style.inputWidth)
      val bg = if (input.focused || input.dragged || isActive) style.focusBackgroundSprite else style.idleBackgroundSprite
      val color = if (enabled) Color.White else Color.White * 0.5

      if (enabled)
        inputs.add(0, input, inputRect, 0.0)

      canvas.draw(0, bg, inputRect, color)

      {
        val textStyle = if (enabled) {
          labelStyle.textStyle
        } else {
          val col = labelStyle.textStyle.color
          val faded = col * 0.5
          labelStyle.textStyle.copy(color = faded)
        }

        canvas.drawText(0, textStyle, labelRect, title)
      }

      if (!isActive) {
        val text = bindToText(getBind)
        canvas.drawText(0, style.textStyle, inputRect, text)
      } else {
        val text = lc"menu.options.pressKey"
        val textStyle = style.textStyle.copy(color = style.textStyle.color * 0.75)
        canvas.drawText(0, textStyle, inputRect, text)
      }
    }
  }

}

