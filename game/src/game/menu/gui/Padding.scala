package menu.gui

import ui.Layout

class Padding(val amount: Double) extends Element {
  def update(parent: Layout): Unit = {
    parent.padTop(amount)
  }
}

