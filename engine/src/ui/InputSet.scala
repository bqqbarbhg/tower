package ui

import platform.AppWindow
import ui.InputSet._

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

  private[ui] case class InputWithLayout(layer: Int, input: InputArea, layout: Layout, index: Int, distance: Double)

  private val BlockerArea = new InputArea()

}

class InputSet {
  private[ui] var updateTick: Int = 0
  private[ui] val inputs = ArrayBuffer[InputWithLayout]()
  private[ui] var focusedInput: InputArea = null
  private[ui] var focusedIndex: Int = -1
  private[ui] var focusedAreaLayout: Layout = null
  private[ui] var didClick: Boolean = false
  private[ui] var prevDown: Boolean = false

  def focused: Option[(InputArea, Int)] = if (focusedInput != null) Some((focusedInput, focusedIndex)) else None
  def focusedArea: Option[Layout] = Option(focusedAreaLayout)

  def clicked: Boolean = didClick

  def add(layer: Int, input: InputArea, layout: Layout, distance: Double = 0.0, index: Int = 0): Unit = {
    inputs += InputWithLayout(layer, input, layout.copy, index, distance)
  }

  def addBlocker(layer: Int, layout: Layout, distance: Double = 0.0): Unit = {
    inputs += InputWithLayout(layer, BlockerArea, layout.copy, 0, distance)
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
    focusedIndex = -1
    focusedAreaLayout = null

    val orderedInputs = inputs.sortBy(input => {
      val base = input.layer * -2
      if (input.input == BlockerArea) base + 1
      else base
    })

    for (input <- inputs) {
      input.input.ownerSet = this
      input.input.updateTick = updateTick
    }

    inputs.clear()

    var bestDist = Double.PositiveInfinity
    for (input <- orderedInputs) {
      val l = input.layout
      var dist = 0.0
      if (mouse.x < l.x0) dist += l.x0 - mouse.x
      if (mouse.y < l.y0) dist += l.y0 - mouse.y
      if (mouse.x > l.x1) dist += mouse.x - l.x1
      if (mouse.y > l.y1) dist += mouse.y - l.y1
      if (dist <= input.distance) {
        if (input.input == BlockerArea) {
          return
        } else if (dist < bestDist) {
          focusedInput = input.input
          focusedIndex = input.index
          focusedAreaLayout = input.layout
          bestDist = dist
        }
      }
    }
  }

}

