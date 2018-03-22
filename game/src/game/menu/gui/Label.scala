package menu.gui

import ui.Canvas.TextStyle
import ui.Layout

case class LabelStyle(height: Double, textStyle: TextStyle)

abstract class Label(val style: LabelStyle) extends Element {
  def text: String

  def update(parent: Layout): Unit = {
    if (visible) {
      val rect = parent.pushTop(style.height)
      canvas.drawText(0, style.textStyle, rect, text)
    }
  }
}
