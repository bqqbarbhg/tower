package menu.gui

import ui.{Canvas, InputSet, Layout}

abstract class Element {
  var canvas: Canvas = _
  var inputs: InputSet = _

  def enabled: Boolean = true
  def visible: Boolean = true

  def update(parent: Layout): Unit
}

