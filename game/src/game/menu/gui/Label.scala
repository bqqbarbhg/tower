package menu.gui

import ui.Canvas.TextStyle
import ui.Layout

case class LabelStyle(height: Double, textStyle: TextStyle)

abstract class Label(val style: LabelStyle) extends Element {
  def text: String

  def update(parent: Layout): Unit = {
    if (visible) {
      val textStyle = if (enabled) {
        style.textStyle
      } else {
        val col = style.textStyle.color
        val faded = col * 0.5
        style.textStyle.copy(color = faded)
      }

      val rect = parent.pushTop(style.height)
      canvas.drawText(0, textStyle, rect, text)
    }
  }
}
