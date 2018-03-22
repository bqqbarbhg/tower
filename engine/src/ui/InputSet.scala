package ui

import platform.AppWindow
import ui.InputSet.{InputArea, InputWithLayout}

import scala.collection.mutable.ArrayBuffer

object InputSet {

  class InputArea {
    private[ui] var ownerSet: InputSet = null
    private[ui] var updateTick: Int = -1

    def exists: Boolean = ownerSet != null && ownerSet.updateTick == updateTick

    def focusIndex: Int = {
      if (!exists || !(ownerSet.focusedInput eq this)) return -1
      ownerSet.focusedIndex
    }
    def focused: Boolean = focusIndex >= 0

    def clickIndex: Int = {
      val index = focusIndex
      if (index < 0 || !ownerSet.didClick) return -1
      index
    }
    def clicked: Boolean = clickIndex >= 0

  }

  private[ui] case class InputWithLayout(input: InputArea, layout: Layout, index: Int)

}

class InputSet {
  private[ui] var updateTick: Int = 0
  private[ui] val inputs = ArrayBuffer[InputWithLayout]()
  private[ui] var focusedInput: InputArea = null
  private[ui] var focusedIndex: Int = -1
  private[ui] var didClick: Boolean = false
  private[ui] var prevDown: Boolean = false

  def clicked: Boolean = didClick

  def add(input: InputArea, layout: Layout, index: Int = 0): Unit = {
    inputs += InputWithLayout(input, layout, index)
  }

  def update(): Unit = {
    updateTick += 1

    val mouse = AppWindow.mousePosition

    if (AppWindow.mouseButtonDown(0)) {
      didClick = !prevDown
      prevDown = true
    } else {
      didClick = false
      prevDown = false
    }

    focusedInput = null

    for (input <- inputs) {
      input.input.ownerSet = this
      input.input.updateTick = updateTick
      if (input.layout.contains(mouse)) {
        focusedInput = input.input
        focusedIndex = input.index
      }
    }
  }

}

