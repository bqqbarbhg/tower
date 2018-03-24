package menu

import menu.OptionsMenu._
import ui.InputSet.InputArea
import ui.Canvas._
import ui._
import asset._
import core._
import game.options.GraphicsOptions.OpenGlOptions
import menu.gui._
import ui.SpriteBatch.SpriteDraw
import game.options._
import main.GameStartup
import platform.AppWindow

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object OptionsMenu {

  val MainFont = FontAsset("font/catamaran/Catamaran-SemiBold.ttf.s2ft")
  val TitleFont = FontAsset("font/catamaran/Catamaran-Bold.ttf.s2ft")
  val TooltipFont = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")

  val TitleLabel = new LabelStyle(26.0, TextStyle(TitleFont, 26.0))
  val NormalLabel = new LabelStyle(22.0, TextStyle(MainFont, 22.0))
  val InfoLabel = new LabelStyle(20.0, TextStyle(MainFont, 20.0, color = Color.White.copy(a = 0.6)))
  val ButtonLabel = new LabelStyle(32.0, TextStyle(MainFont, 26.0, align = AlignCenter))

  val TooltipTextStyle = TextStyle(TooltipFont, 16.0)
  val TabTextStyle = TextStyle(TitleFont, 28.0, align = AlignCenter)
  val MonitorIndexTextStyle = TextStyle(TooltipFont, 26.0, align = AlignCenter)
  val BgSprite = Identifier("gui/menu/background_white.png")

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
    focusBackgroundSprite = Identifier("gui/menu/background_idle.png"),
    checkRadius = 0.0,
    iconPadding = 5.0
  )

  val NormalButton = new ButtonStyle(
    height = 32.0,
    idleBackgroundSprite = Identifier("gui/menu/background_idle.png"),
    focusBackgroundSprite = Identifier("gui/menu/background_focus.png"),
    clickRadius = 0.0,
    padding = 0.0,
  )

  val ColMonitorSelectedBg = Color.rgb(0xDDDDDD)
  val ColMonitorSelectedText = Color.rgb(0x888888)
  val ColMonitorOtherBg = Color.rgb(0x444444)
  val ColMonitorOtherText = Color.rgb(0x888888)
  val ColTooltipInside = Color.rgb(0x666666)
  val ColTooltipBorder = Color.rgb(0x555555)
  val ColTabBg = Color.rgba(0x555555, 0.5)
  val ColTabHover = Color.rgba(0x333333, 0.5)
  val ColTabUnselected = Color.rgba(0x222222, 0.5)
}

class OptionsMenu(val inputs: InputSet, val canvas: Canvas) {

  var options = Options.current.copy

  val tooltips = new mutable.HashMap[InputArea, Int => Option[String]]()
  val customTooltips = new mutable.HashMap[InputArea, (Vector2, Vector2, Int) => Unit]()

  def addTooltip(area: InputArea, tooltip: => String): Unit = {
    tooltips(area) = _ => Some(tooltip)
  }

  def addTooltipOption(area: InputArea, tooltip: => Option[String]): Unit = {
    tooltips(area) = _ => tooltip
  }

  def addTooltipIndexed(area: InputArea, tooltip: Int => Option[String]): Unit = {
    tooltips(area) = tooltip
  }

  def addCustomTooltip(area: InputArea, tooltipFunc: (Vector2, Vector2, Int) => Unit): Unit = {
    customTooltips(area) = tooltipFunc
  }

  def drawTooltipBox(unit: Vector2, pos: Vector2, size: Vector2): Unit = {
    val pad = unit * 5.0
    val outline = unit * 2.0
    val color = ColTooltipInside
    val outlineColor = ColTooltipBorder
    val bgPos = pos - pad
    val bgSize = size + pad * 2.0
    val outlinePos = bgPos - outline
    val outlineSize = bgSize + outline * 2.0

    canvas.draw(4, BgSprite, outlinePos.x, outlinePos.y, outlineSize.x, outlineSize.y, outlineColor)
    canvas.draw(4, BgSprite, bgPos.x, bgPos.y, bgSize.x, bgSize.y, color)
  }

  val glElements = {
    val elements = ArrayBuffer[Element]()
    elements += new Label(TitleLabel) {
      override def text: String = "OpenGL compatability"
    }

    elements += new Padding(10.0)

    elements += new Label(InfoLabel) {
      override def text: String = "Preset"
    }

    elements += new LabelDropdown[String](NormalDropdown, NormalLabel) {
      override val items: Seq[String] = GraphicsOptions.OpenGlOptions.Presets.map(_._1)
      override def currentItem: String = {
        val preset = options.graphics.openGl.preset
        if (preset.isEmpty) "Custom"
        else preset
      }
      override def setItem(newItem: String): Unit = {
        val preset = GraphicsOptions.OpenGlOptions.Presets.find(_._1 == newItem).map(_._2).flatten
        preset match {
          case Some(opts) => options.graphics.openGl = opts()
          case None => options.graphics.openGl.preset = ""
        }
      }

      addTooltipOption(inputOpen, if (!isOpen) Some("Control the way the application interfaces with OpenGL.") else None)
      addTooltipIndexed(inputSelect, index => {
        GraphicsOptions.OpenGlOptions.Presets.map(_._1).toSeq(index) match {
          case "Ancient" => Some(s"Compatible with OpenGL 2 hardware with a few ubiquitous extensions. Has significant performance penalty.")
          case "Compatible" => Some(s"Compatible with standard OpenGL 3 hardware.")
          case "Modern" => Some(s"Uses up to OpenGL 4 features if detected as available.")
          case _ => None
        }
      })
    }

    trait GlOption extends Element {
      override def enabled: Boolean = options.graphics.openGl.preset == ""
    }

    def mapModeTooltip(index: Int): Option[String] = {
      GraphicsOptions.OpenGlOptions.MapModes(index) match {
        case "SubData" => Some("Use the old-style glBufferSubData(). The most compatible option but especially bad for vertex buffers.")
        case "Map" => Some("Use the old-style glMapBufferRange(). Very compatible, but doesn't have the best performance especially for uniform buffers.")
        case "Persistent" => Some("Modern persistent buffer mapping. Writes directly data to the buffer. Should be compatible with most modern video cards. Intended for vertex buffer mappings.")
        case "PersistentCopy" => Some("Like Persistent, but copies the data from a temporary buffer. Intended for uniform buffer mappings.")
        case "PersistentCoherent" => Some("Coherent version of Persistent. Theoretically the fastest for vertex buffers, but may cause issues with some drivers.")
        case "PersistentCopyCoherent" => Some("Coherent version of PersistentCopy. Theoretically the fastest for uniforms, but may cause issues with some drivers.")
        case _ => None
      }
    }

    elements += new Padding(20.0)

    elements += new Label(InfoLabel) with GlOption {
      override def text: String = "Uniform map mode"
    }

    elements += new LabelDropdown[String](NormalDropdown, NormalLabel) with GlOption {
      override def items: Seq[String] = GraphicsOptions.OpenGlOptions.MapModes
      override def currentItem: String = options.graphics.openGl.uniformMapMode
      override def setItem(newItem: String): Unit = options.graphics.openGl.uniformMapMode = newItem

      addTooltipOption(inputOpen, if (!isOpen) Some("Defines how uniform buffer memory is mapped to the application.") else None)
      addTooltipIndexed(inputSelect, mapModeTooltip)
    }

    elements += new Padding(10.0)

    elements += new Label(InfoLabel) with GlOption {
      override def text: String = "Vertex map mode"
    }

    elements += new LabelDropdown[String](NormalDropdown, NormalLabel) with GlOption {
      override def items: Seq[String] = GraphicsOptions.OpenGlOptions.MapModes
      override def currentItem: String = options.graphics.openGl.vertexMapMode
      override def setItem(newItem: String): Unit = options.graphics.openGl.vertexMapMode = newItem

      addTooltipOption(inputOpen, if (!isOpen) Some("Defines how vertex buffer memory is mapped to the application.") else None)
      addTooltipIndexed(inputSelect, mapModeTooltip)
    }

    elements += new Padding(10.0)

    elements += new LabelCheckbox(NormalCheckbox, NormalLabel) with GlOption {
      def text: String = "Uniform buffers"
      def isChecked: Boolean = options.graphics.openGl.useUniformBuffers
      def setChecked(checked: Boolean): Unit =  options.graphics.openGl.useUniformBuffers = checked

      addTooltip(input, "Use uniform buffer objects to transfer data to the shaders. Disabling this may have a significant performance penalty.")
    }

    elements += new LabelCheckbox(NormalCheckbox, NormalLabel) with GlOption {
      def text: String = "Vertex array cache"
      def isChecked: Boolean = options.graphics.openGl.useVaoCache
      def setChecked(checked: Boolean): Unit =  options.graphics.openGl.useVaoCache = checked

      addTooltip(input, "If enabled vertex array state is re-used for multiple draws and frames. Otherwise it's recreated for each draw. Which one is faster depends on driver.")
    }

    elements += new LabelCheckbox(NormalCheckbox, NormalLabel) with GlOption {
      def text: String = "Row-major matrices"
      def isChecked: Boolean = options.graphics.openGl.useRowMajorMatrices
      def setChecked(checked: Boolean): Unit =  options.graphics.openGl.useRowMajorMatrices = checked

      addTooltip(input, "Lay matrices out as row-major in memory, which packs them more nicely. Has some compatability issues with specific drivers.")
    }

    elements += new LabelCheckbox(NormalCheckbox, NormalLabel) with GlOption {
      def text: String = "Immutable texture storage"
      def isChecked: Boolean = options.graphics.openGl.useImmutableTextureStorage
      def setChecked(checked: Boolean): Unit =  options.graphics.openGl.useImmutableTextureStorage = checked

      addTooltip(input, "Use modern immutable storage for allocating texture data.")
    }

    elements += new Padding(30.0)

    elements
  }

  val qualityElements = {
    def monitorHz: Int = {
      val fps = AppWindow.monitorRefreshRate
      // Round 59Hz to 60Hz
      if ((fps + 1) % 10 == 0) fps + 1
      else fps
    }

    val elements = ArrayBuffer[Element]()

    elements += new Label(TitleLabel) {
      override def text: String = "Quality"
    }

    elements += new Padding(10.0)

    elements += new Label(InfoLabel) {
      override def text: String = "Preset"
    }

    elements += new LabelDropdown[String](NormalDropdown, NormalLabel) {
      override val items: Seq[String] = GraphicsOptions.QualityOptions.Presets.map(_._1)
      override def currentItem: String = {
        val preset = options.graphics.quality.preset
        if (preset.isEmpty) "Custom"
        else preset
      }
      override def setItem(newItem: String): Unit = {
        val preset = GraphicsOptions.QualityOptions.Presets.find(_._1 == newItem).map(_._2).flatten
        preset match {
          case Some(opts) => options.graphics.quality = opts()
          case None => options.graphics.quality.preset = ""
        }
      }

      addTooltipOption(inputOpen, if (!isOpen) Some("Rendering quality level, higher levels are more intensive for the system.") else None)
      addTooltipIndexed(inputSelect, index => {
        GraphicsOptions.QualityOptions.Presets.map(_._1).toSeq(index) match {
          case "Minimal" =>
            val fps = monitorHz / 2
            Some(s"Least computationally intensive setup with significant graphical downgrades. Should only be used if absolutely necessary. Framerate limited to ${fps}fps on this screen.")
          case "Low" => Some(s"Low resolution textures and rendering. Drastically simplified shading model.")
          case "Medium" => Some(s"Render at native resolution with the correct shading model, but with slightly lighter textures.")
          case "High" => Some(s"The recommended settings for high-quality rendering.")
          case _ => None
        }
      })
    }

    trait QualityOption extends Element {
      override def enabled: Boolean = options.graphics.quality.preset == ""
    }

    elements += new Padding(20.0)


    elements += new LabelCheckbox(NormalCheckbox, NormalLabel) with QualityOption {
      def text: String = "Vertical sync"
      def isChecked: Boolean = options.graphics.quality.verticalSync
      def setChecked(checked: Boolean): Unit =  options.graphics.quality.verticalSync= checked

      addTooltip(input, {
        val fps = monitorHz
        s"Synchronize framerate with the monitor. Tries to lock to ${fps}fps on this screen."
      })
    }

    elements += new LabelCheckbox(NormalCheckbox, NormalLabel) with QualityOption {
      def text: String = "Half framerate"
      def isChecked: Boolean = options.graphics.quality.halfFramerate
      def setChecked(checked: Boolean): Unit =  options.graphics.quality.halfFramerate = checked

      addTooltip(input, {
        val fps = monitorHz / 2
        s"Synchronizes the framerate with the monitor like vertical sync, but renders only every other frame. Tries to lock to ${fps}fps on this screen."
      })
    }

    elements += new Padding(10.0)

    elements += new Label(InfoLabel) with QualityOption {
      override def text: String = "Texture resolution"
    }

    elements += new LabelDropdown[Int](NormalDropdown, NormalLabel) with QualityOption {
      override val items: Seq[Int] = Array(256, 512, 1024, 2048)
      override def currentItem: Int = options.graphics.quality.maxTextureSize
      override def setItem(newItem: Int): Unit = options.graphics.quality.maxTextureSize = newItem
      override def itemToText(item: Int): String = s"${item} x ${item}"

      addTooltipOption(inputOpen, if (!isOpen) Some("Maximum resolution that is supported for textures. Lower sizes work better with video cards that have less memory.") else None)
    }

    elements
  }

  val commonElements = {
    val elements = ArrayBuffer[Element]()
    val monitors = AppWindow.listMonitors

    elements += new Label(InfoLabel) {
      override def text: String = "Window mode"
    }

    elements += new LabelDropdown[String](NormalDropdown, NormalLabel) {
      override val items: Seq[String] = Options.WindowModes
      override def currentItem: String = options.windowMode
      override def setItem(newItem: String): Unit = options.windowMode = newItem
      override def itemToText(item: String): String = item match {
        case "Window" => "Windowed"
        case "Fullscreen" => "Fullscreen"
        case "Borderless" => "Borderless"
      }

      addTooltipOption(inputOpen, if (!isOpen) Some("What kind of window to use for the game") else None)

      addTooltipIndexed(inputSelect, index => {
        Options.WindowModes(index) match {
          case "Window" => Some("Display the game inside a standard window in the desktop.")
          case "Fullscreen" => Some("Exclusive full-screen display: smoothest rendering but may have troubles with switching to other windows.")
          case "Borderless" => Some("Full-screen window with no border: may be more laggy than real fullscreen, but switching windows should work better.")
          case _ => None
        }
      })
    }

    elements += new Padding(10.0)

    elements += new Label(InfoLabel) {
      override def text: String = "Monitor"
    }

    val monitorNames = "Primary" +: monitors.zipWithIndex.map({ case (m, i) =>
      s"${i + 1}: ${m.align}"
    })

    elements += new LabelDropdown[Int](NormalDropdown, NormalLabel) {
      override val items: Seq[Int] = (0 until monitors.length + 1)
      override def currentItem: Int = options.monitor
      override def setItem(newItem: Int): Unit = options.monitor = newItem
      override def itemToText(item: Int): String = monitorNames(item)

      addTooltipOption(inputOpen, if (!isOpen) Some("Which monitor to display the game on") else None)

      addCustomTooltip(inputSelect, (pos, unit, index) => {
        val size = unit *@ Vector2(200.0, 200.0)

        val ix = index - 1
        if (ix >= 0 && ix < monitors.length) {

          val min = monitors.map(m => m.pos)
            .reduce((a, b) => Vector2(math.min(a.x, b.x), math.min(a.y, b.y)))
          val max = monitors.map(m => m.pos + m.size)
            .reduce((a, b) => Vector2(math.max(a.x, b.x), math.max(a.y, b.y)))

          val areaSize = max - min
          val scaleFactor = if (areaSize.y > areaSize.x) {
            1.0 / areaSize.y
          } else {
            1.0 / areaSize.x
          }
          val scale = unit * 200.0 * scaleFactor

          for ((monitor, monitorIndex) <- monitors.zipWithIndex) {
            val (color, textColor) = if (ix == monitorIndex) {
              (ColMonitorSelectedBg, ColMonitorSelectedText)
            } else {
              (ColMonitorOtherBg, ColMonitorOtherText)
            }

            val pad = Vector2(3.0, 3.0)
            val drawPos = pos + (-min + monitor.pos) *@ scale + pad
            val drawSize = monitor.size *@ scale - pad * 2.0
            val textStyle = MonitorIndexTextStyle.copy(color = textColor).scaled(unit.y)

            canvas.draw(5, BgSprite, drawPos, drawSize, color)
            canvas.drawText(5, textStyle, drawPos, drawSize, (monitorIndex + 1).toString)
          }

          drawTooltipBox(unit, pos, areaSize *@ scale)
        }
      })
    }

  }

  val applyButton = new LabelButton(NormalButton, ButtonLabel) {
    override def text: String = "Apply"
    override def onClick(): Unit = {
      Options.current = options.copy
      GameStartup.restartRequested = true
    }
  }

  val cancelButton = new LabelButton(NormalButton, ButtonLabel) {
    override def text: String = "Cancel"
    override def onClick(): Unit = {
    }
  }

  val separateElements = Vector(applyButton, cancelButton)

  val allElements = commonElements ++ glElements ++ qualityElements ++ separateElements


  abstract class Tab {
    def name: String = ""
    def update(parent: Layout): Unit
  }

  object TabCommon extends Tab {
    override def name = "Common"

    override def update(parent: Layout): Unit = {
      val commonParent = parent.pushLeft(200.0)

      for (element <- commonElements) {
        element.update(commonParent)
      }
    }
  }

  object TabRendering extends Tab {
    override def name = "Graphics"

    override def update(parent: Layout): Unit = {
      val qualityParent = parent.pushLeft(200.0)
      parent.padLeft(50.0)
      val glParent = parent.pushLeft(200.0)

      for (element <- qualityElements) {
        element.update(qualityParent)
      }

      for (element <- glElements) {
        element.update(glParent)
      }
    }

  }

  val TabInput = new InputArea()
  val tabs = Array(TabCommon, TabRendering)
  var activeTab: Tab = TabCommon

  def update(): Unit = {
    val parent = Layout.screen720p.padAround(50.0)
    val unit = parent.unit

    for (element <- allElements) {
      element.inputs = inputs
      element.canvas = canvas
    }


    val top = parent.pushTop(35.0)
    val bottom = parent.pushBottom(50.0)
    bottom.padTop(10.0)

    val clickedTab = TabInput.clickIndex
    if (clickedTab >= 0) {
      activeTab = tabs(clickedTab)
    }

    for ((tab, index) <- tabs.zipWithIndex) {
      val loc = top.pushLeft(150.0)
      loc.padLeft(10.0)

      val color = if (tab == activeTab) {
        ColTabBg
      } else if (TabInput.focusIndex == index) {
        ColTabHover
      } else {
        ColTabUnselected
      }

      canvas.draw(0, BgSprite, loc, color)
      canvas.drawText(0, TabTextStyle, loc, tab.name)

      inputs.add(0, TabInput, loc, 0.0, index)
    }

    canvas.draw(0, BgSprite, parent, ColTabBg)
    val tabArea = parent.copy.padAround(25.0)
    activeTab.update(tabArea)

    val applyPos = bottom.pushLeft(100.0)
    bottom.padLeft(20.0)
    val cancelPos = bottom.pushLeft(100.0)

    applyButton.update(applyPos)
    cancelButton.update(cancelPos)

    for {
      focusedInput <- inputs.focused
      focusedArea <- inputs.focusedArea
    } {
      for {
        maybeTooltip <- tooltips.get(focusedInput._1)
        tooltip <- maybeTooltip(focusedInput._2)
      } {
        val pos = Vector2(focusedArea.x1, focusedArea.y0) + unit *@ Vector2(15.0, 5.0)
        val width = unit.x * 200.0
        val bounds = Vector2(width, unit.y * 1000.0)
        val style = TooltipTextStyle.scaled(unit.y)
        val endY = canvas.drawTextWrapped(5, style, pos, bounds, tooltip)
        val bgSize = Vector2(width, endY - pos.y)
        drawTooltipBox(unit, pos, bgSize)
      }

      for {
        customTooltip <- customTooltips.get(focusedInput._1)
      } {
        val pos = Vector2(focusedArea.x1, focusedArea.y0) + unit *@ Vector2(15.0, 5.0)
        customTooltip(pos, unit, focusedInput._2)
      }
    }


  }

}
