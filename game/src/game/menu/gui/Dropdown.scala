package menu.gui

import core._
import ui.InputSet.InputArea
import ui.Layout

case class DropdownStyle(iconSize: Double,
                         openRadius: Double,
                         selectRadius: Double,
                         blockRadius: Double,
                         idleBackgroundSprite: Identifier,
                         focusBackgroundSprite: Identifier,
                         itemBackgroundSprite: Identifier,
                         focusedItemBackgroundSprite: Identifier,
                         iconSprite: Identifier,
                         itemPadding: Double,
                        )

abstract class DropdownBase(val style: DropdownStyle) extends Element {
  def numItems: Int
  def setIndex(index: Int): Unit
  def drawCurrentItem(parent: Layout): Layout
  def drawItemAtIndex(parent: Layout, index: Int): Layout

  var isOpen: Boolean = false

  val inputOpen = new InputArea()
  val inputSelect = new InputArea()

  override def update(parent: Layout): Unit = {
    if (enabled) {
      if (inputOpen.clicked) {
        isOpen = !isOpen
      } else if (inputSelect.clicked) {
        setIndex(inputSelect.clickIndex)
        isOpen = false
      } else if (inputs.clicked) {
        isOpen = false
      }

    } else {
      isOpen = false
    }

    if (visible) {
      val fullRect = parent.pushTop(style.iconSize)
      val padded = fullRect.copy.padLeft(style.itemPadding)
      val icon = fullRect.copy.pushRight(style.iconSize)
      drawCurrentItem(padded.copy)

      if (enabled)
        inputs.add(0, inputOpen, fullRect, style.openRadius)

      val bg = if (inputOpen.focused || inputOpen.dragged) style.focusBackgroundSprite else style.idleBackgroundSprite
      val color = if (enabled) Color.White else Color.White * 0.5

      canvas.draw(0, bg, fullRect, color)
      canvas.draw(0, style.iconSprite, icon, color)

      if (isOpen) {
        val box = padded.edgeBottom
        var num = numItems
        var focusedBox: Option[Layout] = None
        for (i <- 0 until num) {
          val itemBox = drawItemAtIndex(box, i)
          if (inputSelect.focusIndex == i)
            focusedBox = Some(itemBox)
          if (enabled)
            inputs.add(1, inputSelect, itemBox, style.selectRadius, i)
        }
        box.x0 = fullRect.x0
        box.y0 = fullRect.y1
        inputs.addBlocker(1, box, style.blockRadius)
        canvas.draw(1, style.itemBackgroundSprite, box)
        for (box <- focusedBox) {
          box.x0 = fullRect.x0
          canvas.draw(1, style.focusedItemBackgroundSprite, box)
        }
      }
    }
  }
}

abstract class Dropdown[T](style: DropdownStyle) extends DropdownBase(style) {
  def items: Seq[T]
  def currentItem: T
  def setItem(newItem: T): Unit
  def drawItem(parent: Layout, layer: Int, item: T): Layout

  override def numItems: Int = items.length
  override def setIndex(index: Int) = setItem(items(index))
  def drawCurrentItem(parent: Layout): Layout = drawItem(parent, 0, currentItem)
  def drawItemAtIndex(parent: Layout, index: Int): Layout = drawItem(parent, 2, items(index))
}

abstract class LabelDropdown[T](style: DropdownStyle, val labelStyle: LabelStyle) extends Dropdown[T](style) {
  def itemToText(item: T): String = item.toString

  def drawItem(parent: Layout, layer: Int, item: T): Layout = {
    val rect = parent.pushTop(labelStyle.height)
    val textStyle = if (enabled) {
      labelStyle.textStyle
    } else {
      val col = labelStyle.textStyle.color
      val faded = col * 0.5
      labelStyle.textStyle.copy(color = faded)
    }
    canvas.drawText(layer, textStyle, rect, itemToText(item))
    rect
  }
}

