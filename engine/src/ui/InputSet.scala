package ui

import core._
import platform.AppWindow
import ui.InputSet._

import scala.collection.mutable.ArrayBuffer

object InputSet {

  class InputArea {
    private[ui] var ownerSet: InputSet = null
    private[ui] var updateTick: Int = -1

    def exists: Boolean = ownerSet != null && ownerSet.updateTick == updateTick

    def hoverIndex: Int = {
      if (!exists || !(ownerSet.hoveredInput eq this)) return -1
      ownerSet.hoveredIndex
    }
    def hovered: Boolean = hoverIndex >= 0

    def dragIndex: Int = {
      if (!exists || !(ownerSet.draggedInput eq this)) return -1
      ownerSet.draggedIndex
    }
    def dragged: Boolean = dragIndex >= 0

    def clickIndex: Int = {
      val index = focusIndex
      if (index < 0 || !ownerSet.didClick) return -1
      index
    }
    def clicked: Boolean = clickIndex >= 0

    def focusIndex: Int = {
      val hoverIx = hoverIndex
      if (hoverIx >= 0) return hoverIx
      dragIndex
    }
    def focused: Boolean = focusIndex >= 0

  }

  private[ui] case class InputWithLayout(layer: Int, input: InputArea, layout: Layout, index: Int, distance: Double)

  private val BlockerArea = new InputArea()

}

class InputSet {
  private[ui] var updateTick: Int = 0
  private[ui] val inputs = ArrayBuffer[InputWithLayout]()

  private[ui] var hoveredInput: InputArea = null
  private[ui] var hoveredIndex: Int = -1
  private[ui] var hoveredAreaLayout: Layout = null
  private[ui] var hoveredLayerIndex: Int = Int.MinValue

  private[ui] var draggedInput: InputArea = null
  private[ui] var draggedIndex: Int = -1
  private[ui] var draggedAreaLayout: Layout = null
  private[ui] var draggedLayerIndex: Int = Int.MinValue

  private[ui] var didClick: Boolean = false
  private[ui] var prevDown: Boolean = false


  def hovered: Option[(InputArea, Int)] = if (hoveredInput != null) Some((hoveredInput, hoveredIndex)) else None
  def hoveredArea: Option[Layout] = Option(hoveredAreaLayout)
  def hoveredLayer: Int = hoveredLayerIndex

  def dragged: Option[(InputArea, Int)] = if (draggedInput != null) Some((draggedInput, draggedIndex)) else None
  def draggedArea: Option[Layout] = Option(draggedAreaLayout)
  def draggedLayer: Int = draggedLayerIndex

  def focused: Option[(InputArea, Int)] = hovered.orElse(dragged)
  def focusedArea: Option[Layout] = hoveredArea.orElse(draggedArea)
  def focusedLayer: Int = math.max(hoveredLayerIndex, draggedLayerIndex)

  def dragPosition: Vector2 = AppWindow.mousePosition

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

    val mouseDown = AppWindow.mouseButtonDown(0)
    val prevPrevDown = prevDown
    if (mouseDown) {
      didClick = !prevDown
      prevDown = true
    } else {
      didClick = false
      prevDown = false

      draggedInput = null
      draggedIndex = -1
      draggedAreaLayout = null
      draggedLayerIndex = Int.MinValue
    }

    hoveredInput = null
    hoveredIndex = -1
    hoveredAreaLayout = null
    hoveredLayerIndex = Int.MinValue

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

    def findFocused(): Unit = {
      var bestDist = Double.PositiveInfinity
      for (input <- orderedInputs) {
        val l = input.layout
        var dist = 0.0

        if (mouse.x < l.x0) dist += l.x0 - mouse.x
        if (mouse.y < l.y0) dist += l.y0 - mouse.y
        if (mouse.x > l.x1) dist += mouse.x - l.x1
        if (mouse.y > l.y1) dist += mouse.y - l.y1
        if (dist <= input.distance) {
          hoveredLayerIndex = input.layer
          if (input.input == BlockerArea) {
            return
          } else if (dist < bestDist) {
            hoveredInput = input.input
            hoveredIndex = input.index
            hoveredAreaLayout = input.layout
            bestDist = dist
          }
        }
      }
    }

    if (!(mouseDown && prevPrevDown))
      findFocused()

    if (didClick) {
      draggedInput = hoveredInput
      draggedIndex = hoveredIndex
      draggedAreaLayout = hoveredAreaLayout
      draggedLayerIndex = hoveredLayerIndex
    }
  }

}

