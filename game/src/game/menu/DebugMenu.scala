package menu

import scala.collection.mutable.ArrayBuffer

import core._
import io.property._
import ui._
import DebugMenu._
import asset._
import menu.gui._
import ui.Canvas._

object DebugMenu {
  val MainFont = FontAsset("font/catamaran/Catamaran-SemiBold.ttf.s2ft")
  val NormalLabel = new LabelStyle(22.0, TextStyle(MainFont, 22.0))
  val InfoLabel = new LabelStyle(20.0, TextStyle(MainFont, 20.0, color = Color.White.copy(a = 0.6)))

  val NormalCheckbox = new CheckboxStyle(22.0,
    Identifier("gui/menu/checkbox_false.png"),
    Identifier("gui/menu/checkbox_true.png"),
    focusBackgroundSprite = Identifier("gui/menu/background_idle.png"),
    checkRadius = 0.0,
    iconPadding = 5.0
  )

  val NormalDropdown = new DropdownStyle(22.0, 0.0, 0.0, 0.0,
    idleBackgroundSprite = Identifier("gui/menu/background_idle.png"),
    focusBackgroundSprite = Identifier("gui/menu/background_focus.png"),
    itemBackgroundSprite = Identifier("gui/menu/background.png"),
    focusedItemBackgroundSprite = Identifier("gui/menu/background_dropdown_select.png"),
    iconSprite = Identifier("gui/menu/dropdown_icon.png"),
    itemPadding = 5.0
  )
}

class DebugMenu(val inputs: InputSet, val canvas: Canvas, val obj: PropertyContainer) {

  val elements = new ArrayBuffer[gui.Element]()

  val propSet = obj.propertySet
  for ((prop, index) <- propSet.properties.zipWithIndex) {
    if (index != 0)
      elements += new Padding(10.0)

    prop match {
      case p: BoolProp =>
        elements += new LabelCheckbox(NormalCheckbox, NormalLabel) {
          override def text: String = p.name
          override def isChecked: Boolean = p.get(obj)
          override def setChecked(checked: Boolean): Unit = p.set(obj, checked)
        }
      case p =>
        val enums = propSet.enumerations.get(p.name)
        enums match {
          case Some(values) =>
            elements += new Label(InfoLabel) {
              override def text: String = p.name
            }

            elements += new LabelDropdown[Any](NormalDropdown, NormalLabel) {
              override def items: Seq[Any] = values
              override def currentItem: Any = p.getGeneric(obj)
              override def setItem(newItem: Any): Unit = p.setGeneric(obj, newItem)
            }

          case None =>
            elements += new Label(NormalLabel) {
              override val text: String = s"${p.name}: ${p.getGeneric(obj).toString}"
            }
        }
    }
  }

  def update(parent: Layout): Unit = {
    for (element <- elements) {
      element.canvas = canvas
      element.inputs = inputs
    }

    for (element <- elements) {
      element.update(parent)
    }
  }

}

