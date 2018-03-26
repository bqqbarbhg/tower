package menu.gui

import core._
import ui.InputSet.InputArea
import ui.Layout

case class CheckboxStyle(iconSize: Double,
                         uncheckedSprite: Identifier,
                         checkedSprite: Identifier,
                         focusBackgroundSprite: Identifier,
                         checkRadius: Double,
                         iconPadding: Double,
                        )

abstract class Checkbox(val style: CheckboxStyle) extends Element {
  def drawContent(parent: Layout): Layout
  def isChecked: Boolean
  def setChecked(checked: Boolean): Unit

  val input = new InputArea()

  override def update(parent: Layout): Unit = {
    if (enabled) {
      if (input.clicked)
        setChecked(!isChecked)
    }

    if (visible) {
      val rect = parent.pushTop(style.iconSize)
      val fullRect = rect.copy

      if (enabled)
        inputs.add(0, input, rect, style.checkRadius)

      val icon = rect.pushLeft(style.iconSize)
      rect.padLeft(style.iconPadding)
      val sprite = if (isChecked) style.checkedSprite else style.uncheckedSprite

      val color = if (enabled) Color.White else Color.White * 0.5

      if (input.focused || input.dragged) {
        val bg = style.focusBackgroundSprite
        canvas.draw(0, bg, fullRect)
      }

      canvas.draw(0, sprite, icon, color)
      drawContent(rect.copy)
    }
  }
}

abstract class LabelCheckbox(style: CheckboxStyle, val labelStyle: LabelStyle) extends Checkbox(style) {
  def text: String

  def drawContent(parent: Layout): Layout = {
    val rect = parent.pushTop(labelStyle.height)
    val textStyle = if (enabled) {
      labelStyle.textStyle
    } else {
      val col = labelStyle.textStyle.color
      val faded = col * 0.5
      labelStyle.textStyle.copy(color = faded)
    }

    canvas.drawText(0, textStyle, rect, text)
    rect
  }
}

