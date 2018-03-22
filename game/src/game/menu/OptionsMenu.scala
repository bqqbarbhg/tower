package menu

import menu.OptionsMenu._
import ui.InputSet.InputArea
import ui.Canvas.TextStyle
import ui._
import asset._
import core._
import menu.gui._
import ui.SpriteBatch.SpriteDraw
import game.options._
import main.GameStartup

import scala.collection.mutable.ArrayBuffer


object OptionsMenu {

  val MainFont = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")
  val InfoLabel = new LabelStyle(18.0, TextStyle(MainFont, color = Color.White.copy(a = 0.6)))
  val NormalLabel = new LabelStyle(22.0, TextStyle(MainFont))
  val NormalDropdown = new DropdownStyle(22.0, 0.0, 0.0, 0.0,
    idleBackgroundSprite = Identifier("gui/menu/background_idle.png"),
    focusBackgroundSprite = Identifier("gui/menu/background_focus.png"),
    itemBackgroundSprite = Identifier("gui/menu/background.png"),
    focusedItemBackgroundSprite = Identifier("gui/menu/background_dropdown_select.png"),
    iconSprite = Identifier("gui/menu/dropdown_icon.png"),
    itemPadding = 5.0
  )
  val NormalCheckbox = new CheckboxStyle(22.0,
    Identifier("gui/menu/checkbox_false.png"),
    Identifier("gui/menu/checkbox_true.png"),
    checkRadius = 0.0,
    iconPadding = 5.0
  )
  val NormalButton = new ButtonStyle(
    height = 22.0,
    idleBackgroundSprite = Identifier("gui/menu/background_idle.png"),
    focusBackgroundSprite = Identifier("gui/menu/background_focus.png"),
    clickRadius = 0.0,
    padding = 5.0,
  )

}

class OptionsMenu(val inputs: InputSet, val canvas: Canvas) {

  val elements = ArrayBuffer[Element]()
  var options = Options.current.copy

  elements += new Label(NormalLabel) {
    override def text: String = "Uniform map mode"
  }

  elements += new LabelDropdown[String](NormalDropdown, NormalLabel) {
    override def items: Seq[String] = GraphicsOptions.OpenGlOptions.MapModes
    override def currentItem: String = options.graphics.openGl.uniformMapMode
    override def setItem(newItem: String): Unit = options.graphics.openGl.uniformMapMode = newItem
  }

  elements += new Padding(20.0)

  elements += new Label(NormalLabel) {
    override def text: String = "Vertex map mode"
  }

  elements += new LabelDropdown[String](NormalDropdown, NormalLabel) {
    override def items: Seq[String] = GraphicsOptions.OpenGlOptions.MapModes
    override def currentItem: String = options.graphics.openGl.vertexMapMode
    override def setItem(newItem: String): Unit = options.graphics.openGl.vertexMapMode = newItem
  }

  elements += new Padding(20.0)

  elements += new LabelButton(NormalButton, NormalLabel) {
    override def text: String = "Apply"
    override def onClick(): Unit = {
      Options.current = options.copy
      GameStartup.restartRequested = true
    }
  }

  def update(): Unit = {

    val parent = Layout.screen720p.padAround(50.0).pushLeft(250.0)

    for (element <- elements) {
      element.inputs = inputs
      element.canvas = canvas
    }

    for (element <- elements) {
      element.update(parent)
    }

  }

}
