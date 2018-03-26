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
  val TextFont = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")

  val NormalLabel = new LabelStyle(22.0, TextStyle(MainFont, 22.0))
  val InfoLabel = new LabelStyle(20.0, TextStyle(MainFont, 20.0, color = Color.White.copy(a = 0.6)))
  val SectionLabel = new LabelStyle(26.0, TextStyle(MainFont, 26.0))
  val TitleLabel = new LabelStyle(34.0, TextStyle(MainFont, 34.0))

  val NormalCheckbox = new CheckboxStyle(22.0,
    Identifier("gui/menu/checkbox_false.png"),
    Identifier("gui/menu/checkbox_true.png"),
    focusBackgroundSprite = Identifier("gui/menu/background_idle.png"),
    checkRadius = 0.0,
    iconPadding = 5.0
  )

  val NormalTextBox = new TextBoxStyle(22.0,
    textStyle = TextStyle(TextFont, 20.0),
    selectedTextStyle = TextStyle(TextFont, 20.0, color = Color.rgb(0x222222)),
    idleBackgroundSprite = Identifier("gui/menu/background_idle.png"),
    selectSprite = Identifier("gui/menu/background_white.png"),
    paddingLeft = 5.0,
    yOffset = 2.0,
    selectPad = 4.0,
    caretWidth = 2.0,
    doubleClickIntervalSeconds = 0.2,
    selectAllOnFirstClick = false,
  )

  val SliderTextBox = new TextBoxStyle(22.0,
    textStyle = TextStyle(TextFont, 18.0),
    selectedTextStyle = TextStyle(TextFont, 18.0, color = Color.rgb(0x222222)),
    idleBackgroundSprite = Identifier("gui/menu/background_idle.png"),
    selectSprite = Identifier("gui/menu/background_white.png"),
    paddingLeft = 3.0,
    yOffset = 3.0,
    selectPad = 4.0,
    caretWidth = 2.0,
    doubleClickIntervalSeconds = 0.2,
    selectAllOnFirstClick = true,
  )

  val NormalDropdown = new DropdownStyle(22.0, 0.0, 0.0, 0.0,
    idleBackgroundSprite = Identifier("gui/menu/background_idle.png"),
    focusBackgroundSprite = Identifier("gui/menu/background_focus.png"),
    itemBackgroundSprite = Identifier("gui/menu/background.png"),
    focusedItemBackgroundSprite = Identifier("gui/menu/background_dropdown_select.png"),
    iconSprite = Identifier("gui/menu/dropdown_icon.png"),
    itemPadding = 5.0
  )

  val DoubleSlider = new SliderStyle(22.0, 60.0, 5.0,
    rectSprite = Identifier("gui/menu/background_white.png"),
    stringFormat = v => f"$v%.2f",
    lineWidth = 2.0,
    markWidth = 4.0,
  )

  case class ProductBind(parent: Int, child: Int, prop: ProductProp)
}

class DebugMenu(val inputs: InputSet, val canvas: Canvas, val propObj: PropertyContainer) {

  val elements = new ArrayBuffer[gui.Element]()
  val objects = ArrayBuffer[PropertyContainer](propObj)
  val productBinds = ArrayBuffer[ProductBind]()

  val parentPropSet = propObj.propertySet

  elements += new Label(TitleLabel) {
    override val text: String = propObj.propertySet.name
  }

  def addPropSet(propSet: PropertySet, index: Int, prefix: String): Unit = {
    def obj: PropertyContainer = objects(index)

    def addProp(prop: Property): Unit = {
      val fullName = prefix + prop.name

      for (values <- parentPropSet.enumerations.get(fullName)) {
        elements += new Label(InfoLabel) {
          override def text: String = prop.name
        }

        elements += new LabelDropdown[Any](NormalDropdown, NormalLabel) {
          override def items: Seq[Any] = values
          override def currentItem: Any = prop.getGeneric(obj)
          override def setItem(newItem: Any): Unit = prop.setGeneric(obj, newItem)
        }
        return
      }

      for ((genMin, genMax) <- parentPropSet.ranges.get(fullName)) {
        prop match {
          case p: DoubleProp =>
            val min = genMin.asInstanceOf[Double]
            val max = genMax.asInstanceOf[Double]

            elements += new Label(InfoLabel) {
              override def text: String = prop.name
            }

            elements += new Slider(DoubleSlider, SliderTextBox) {
              override def minValue: Double = min
              override def maxValue: Double = max
              override def currentValue: Double = p.get(obj)
              override def setValue(newValue: Double): Unit = p.set(obj, newValue)
              override def step: Option[Double] = None
            }

            return

          case _ =>
        }
      }

      prop match {
        case p: BoolProp =>
          elements += new LabelCheckbox(NormalCheckbox, NormalLabel) {
            override def text: String = p.name
            override def isChecked: Boolean = p.get(obj)
            override def setChecked(checked: Boolean): Unit = p.set(obj, checked)
          }

        case p: StringProp =>
          elements += new Label(InfoLabel) {
            override def text: String = prop.name
          }

          elements += new TextBox(NormalTextBox) {
            override def currentText: String = p.get(obj)
            override def setText(newValue: String): Unit = p.set(obj, newValue)
          }

        case p: ProductProp =>
          elements += new Label(SectionLabel) {
            override val text: String = p.name
          }

          val childIx = objects.length
          objects += p.getProductInstance(obj)
          productBinds += ProductBind(index, childIx, p)
          addPropSet(p.propertySet, childIx, fullName + ".")

        case p =>
          elements += new Label(NormalLabel) {
            override val text: String = s"${p.name}: ${p.getGeneric(obj).toString}"
          }
      }
    }

    for ((prop, propIndex) <- propSet.properties.zipWithIndex) {
      if (propIndex != 0)
        elements += new Padding(5.0)

      addProp(prop)
    }
  }

  addPropSet(propObj.propertySet, 0, "")

  def update(parent: Layout): Unit = {
    for (element <- elements) {
      element.canvas = canvas
      element.inputs = inputs
    }

    for (bind <- productBinds) {
      objects(bind.child) = bind.prop.getProductInstance(objects(bind.parent))
    }

    for (element <- elements) {
      element.update(parent)
    }

    for (bind <- productBinds) {
      bind.prop.setProductInstance(objects(bind.parent), objects(bind.child))
    }
  }

}

