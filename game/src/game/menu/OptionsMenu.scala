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
import locale.LocaleInfo
import main.GameStartup
import platform.AppWindow
import locale.Locale
import locale.LocaleString._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try


object OptionsMenu {

  val MainFont = FontAsset("font/catamaran/Catamaran-SemiBold.ttf.s2ft")
  val TitleFont = FontAsset("font/catamaran/Catamaran-Bold.ttf.s2ft")
  val TooltipFont = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")
  val TextBoxFont = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")

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

  val SliderTextBox = new TextBoxStyle(22.0,
    textStyle = TextStyle(TextBoxFont, 18.0),
    selectedTextStyle = TextStyle(TextBoxFont, 18.0, color = Color.rgb(0x222222)),
    idleBackgroundSprite = Identifier("gui/menu/background_idle.png"),
    selectSprite = Identifier("gui/menu/background_white.png"),
    paddingLeft = 3.0,
    yOffset = 3.0,
    selectPad = 4.0,
    caretWidth = 2.0,
    doubleClickIntervalSeconds = 0.2,
    selectAllOnFirstClick = true,
  )

  val NormalSlider = new SliderStyle(22.0, 60.0, 5.0,
    rectSprite = Identifier("gui/menu/background_white.png"),
    stringFormat = v => f"$v%.2f",
    stringParse = v => Try(v.toDouble).toOption,
    lineWidth = 2.0,
    markWidth = 4.0,
  )

  val PercentageSlider = NormalSlider.copy(
    stringFormat = v => s"${(v * 100.0).toInt}%",
    stringParse = v => Try(v.stripSuffix("%").toDouble / 100.0).toOption,
  )

  val MillisecondSlider = NormalSlider.copy(
    stringFormat = v => s"${(v * 1000.0).toInt}ms",
    stringParse = v => Try(v.toLowerCase.stripSuffix("ms").toDouble / 1000.0).toOption,
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

  case class Tooltip(textFunc: Int => Option[String], overrideLayout: Option[() => Layout] = None)

  val tooltips = new mutable.HashMap[InputArea, Tooltip]()
  val customTooltips = new mutable.HashMap[InputArea, (Vector2, Vector2, Int) => Unit]()

  def addTooltip(area: InputArea, tooltip: => String): Unit = {
    tooltips(area) = Tooltip(_ => Some(tooltip))
  }

  def addTooltipOption(area: InputArea, tooltip: => Option[String]): Unit = {
    tooltips(area) = Tooltip(_ => tooltip)
  }

  def addTooltipIndexed(area: InputArea, tooltip: Int => Option[String]): Unit = {
    tooltips(area) = Tooltip(tooltip)
  }

  def addTooltipToLayout(area: InputArea, layout: => Layout, tooltip: => String): Unit = {
    tooltips(area) = Tooltip(_ => Some(tooltip), Some(() => layout))
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

  abstract class SimpleCheckbox(val localeKey: String) extends LabelCheckbox(NormalCheckbox, NormalLabel) {
    override val text: String = lc"menu.options.$localeKey.title"
    private val desc = lc"menu.options.$localeKey.desc"

    addTooltip(input, desc)
  }

  val glElements = {
    val elements = ArrayBuffer[Element]()
    elements += new Label(TitleLabel) {
      override val text: String = lc"menu.options.graphics.gl.title"
    }

    elements += new Padding(10.0)

    elements += new Label(InfoLabel) {
      override val text: String = lc"menu.options.preset"
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

      override def itemToText(item: String): String = if (item == "Custom") lc"menu.options.custom"
      else lc"menu.options.graphics.gl.preset.$item.title"

      addTooltipOption(inputOpen, if (!isOpen) Some(lc"menu.options.graphics.gl.desc") else None)
      addTooltipIndexed(inputSelect, index => {
        val item = GraphicsOptions.OpenGlOptions.Presets.map(_._1).toSeq(index)
        Locale.getSimpleOption(s"menu.options.graphics.gl.preset.$item.desc")
      })
    }

    trait GlOption extends Element {
      override def enabled: Boolean = options.graphics.openGl.preset == ""
    }

    def mapModeTooltip(index: Int): Option[String] = {
      val name = GraphicsOptions.OpenGlOptions.MapModes(index)
      Locale.getSimpleOption(s"menu.options.graphcis.gl.mapMode.$name")
    }

    elements += new Padding(20.0)

    elements += new Label(InfoLabel) with GlOption {
      override val text: String = lc"menu.options.graphics.gl.uniformMapMode.title"
    }

    elements += new LabelDropdown[String](NormalDropdown, NormalLabel) with GlOption {
      override def items: Seq[String] = GraphicsOptions.OpenGlOptions.MapModes
      override def currentItem: String = options.graphics.openGl.uniformMapMode
      override def setItem(newItem: String): Unit = options.graphics.openGl.uniformMapMode = newItem

      addTooltipOption(inputOpen, if (!isOpen) Some(lc"menu.options.graphics.gl.uniformMapMode.desc") else None)
      addTooltipIndexed(inputSelect, mapModeTooltip)
    }

    elements += new Padding(10.0)

    elements += new Label(InfoLabel) with GlOption {
      override val text: String = lc"menu.options.graphics.gl.vertexMapMode.title"
    }

    elements += new LabelDropdown[String](NormalDropdown, NormalLabel) with GlOption {
      override def items: Seq[String] = GraphicsOptions.OpenGlOptions.MapModes
      override def currentItem: String = options.graphics.openGl.vertexMapMode
      override def setItem(newItem: String): Unit = options.graphics.openGl.vertexMapMode = newItem

      addTooltipOption(inputOpen, if (!isOpen) Some(lc"menu.options.graphics.gl.vertexMapMode.desc") else None)
      addTooltipIndexed(inputSelect, mapModeTooltip)
    }

    elements += new Padding(10.0)

    elements += new SimpleCheckbox("graphics.gl.uniformBuffers") with GlOption {
      def isChecked: Boolean = options.graphics.openGl.useUniformBuffers
      def setChecked(checked: Boolean): Unit =  options.graphics.openGl.useUniformBuffers = checked
    }

    elements += new SimpleCheckbox("graphics.gl.useVaoCache") with GlOption {
      def isChecked: Boolean = options.graphics.openGl.useVaoCache
      def setChecked(checked: Boolean): Unit =  options.graphics.openGl.useVaoCache = checked
    }

    elements += new SimpleCheckbox("graphics.gl.useRowMajorMatrices") with GlOption {
      def isChecked: Boolean = options.graphics.openGl.useRowMajorMatrices
      def setChecked(checked: Boolean): Unit =  options.graphics.openGl.useRowMajorMatrices = checked
    }

    elements += new SimpleCheckbox("graphics.gl.useImmutableTextureStorage") with GlOption {
      def isChecked: Boolean = options.graphics.openGl.useImmutableTextureStorage
      def setChecked(checked: Boolean): Unit =  options.graphics.openGl.useImmutableTextureStorage = checked
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
      override def text: String = lc"menu.options.graphics.ql.title"
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
      override def itemToText(item: String): String = if (item == "Custom") lc"menu.options.custom"
      else lc"menu.options.graphics.ql.preset.$item.title"

      addTooltipOption(inputOpen, if (!isOpen) Some(lc"menu.options.graphics.ql.desc") else None)
      addTooltipIndexed(inputSelect, index => {
        GraphicsOptions.QualityOptions.Presets.map(_._1).toSeq(index) match {
          case "Minimal" =>
            val fps = monitorHz / 2
            Locale.getExpressionOption("menu.options.graphics.ql.preset.Minimal.desc", "fps" -> fps.toString)
          case other => Locale.getSimpleOption(s"menu.options.graphics.ql.preset.$other.desc")
        }
      })
    }

    trait QualityOption extends Element {
      override def enabled: Boolean = options.graphics.quality.preset == ""
    }

    elements += new Padding(20.0)


    elements += new LabelCheckbox(NormalCheckbox, NormalLabel) with QualityOption {
      val text: String = lc"menu.options.graphics.ql.vsync.title"
      def isChecked: Boolean = options.graphics.quality.verticalSync
      def setChecked(checked: Boolean): Unit =  options.graphics.quality.verticalSync = checked

      addTooltipOption(input, {
        val fps = monitorHz
        Locale.getExpressionOption("menu.options.graphics.ql.vsync.desc", "fps" -> fps.toString)
      })
    }

    elements += new LabelCheckbox(NormalCheckbox, NormalLabel) with QualityOption {
      val text: String = lc"menu.options.graphics.ql.halfFps.title"
      def isChecked: Boolean = options.graphics.quality.halfFramerate
      def setChecked(checked: Boolean): Unit =  options.graphics.quality.halfFramerate = checked

      addTooltipOption(input, {
        val fps = monitorHz / 2
        Locale.getExpressionOption("menu.options.graphics.ql.halfFps.desc", "fps" -> fps.toString)
      })
    }

    elements += new LabelCheckbox(NormalCheckbox, NormalLabel) with QualityOption {
      val text: String = lc"menu.options.graphics.ql.highBitdepth.title"
      def isChecked: Boolean = options.graphics.quality.highBitdepth
      def setChecked(checked: Boolean): Unit =  options.graphics.quality.highBitdepth = checked

      addTooltipOption(input, Locale.getSimpleOption("menu.options.graphics.ql.highBitdepth.desc"))
    }

    elements += new Padding(10.0)

    elements += new Label(InfoLabel) with QualityOption {
      val text: String = lc"menu.options.graphics.ql.textureSize.title"
    }

    elements += new LabelDropdown[Int](NormalDropdown, NormalLabel) with QualityOption {
      override val items: Seq[Int] = Array(256, 512, 1024, 2048)
      override def currentItem: Int = options.graphics.quality.maxTextureSize
      override def setItem(newItem: Int): Unit = options.graphics.quality.maxTextureSize = newItem
      override def itemToText(item: Int): String = item match {
        case 256 => lc"menu.options.generic.Minimal"
        case 512 => lc"menu.options.generic.Low"
        case 1024 => lc"menu.options.generic.Medium"
        case 2048 => lc"menu.options.generic.High"
        case other => other.toString
      }

      addTooltipOption(inputOpen, if (!isOpen) Some(lc"menu.options.graphics.ql.textureSize.desc") else None)
    }

    elements += new Padding(10.0)

    elements += new Label(InfoLabel) with QualityOption {
      val text: String = lc"menu.options.graphics.ql.shadingQuality.title"
    }

    elements += new LabelDropdown[Int](NormalDropdown, NormalLabel) with QualityOption {
      override val items: Seq[Int] = Array(0, 1, 2, 3)
      override def currentItem: Int = options.graphics.quality.shaderQuality
      override def setItem(newItem: Int): Unit = options.graphics.quality.shaderQuality = newItem
      override def itemToText(item: Int): String = item match {
        case 0 => lc"menu.options.generic.Minimal"
        case 1 => lc"menu.options.generic.Low"
        case 2 => lc"menu.options.generic.Medium"
        case 3 => lc"menu.options.generic.High"
        case other => other.toString
      }

      addTooltipOption(inputOpen, if (!isOpen) Some(lc"menu.options.graphics.ql.shadingQuality.desc") else None)
    }

    elements += new Padding(10.0)

    elements += new Label(InfoLabel) with QualityOption {
      val text: String = lc"menu.options.graphics.ql.antialias.title"
    }

    elements += new LabelDropdown[Int](NormalDropdown, NormalLabel) with QualityOption {
      override val items: Seq[Int] = Array(1, 2, 4, 8, 16)
      override def currentItem: Int = options.graphics.quality.antialias
      override def setItem(newItem: Int): Unit = options.graphics.quality.antialias = newItem
      override def itemToText(item: Int): String = item match {
        case 1  => lc"menu.options.graphics.ql.antialias.None.title"
        case 2  => lc"menu.options.graphics.ql.antialias.Msaa2.title"
        case 4  => lc"menu.options.graphics.ql.antialias.Msaa4.title"
        case 8  => lc"menu.options.graphics.ql.antialias.Msaa8.title"
        case 16 => lc"menu.options.graphics.ql.antialias.Msaa16.title"
        case other => other.toString
      }

      addTooltipOption(inputOpen, if (!isOpen) Some(lc"menu.options.graphics.ql.antialias.desc") else None)
    }

    elements += new Padding(10.0)

    elements += new Label(InfoLabel) with QualityOption {
      val text: String = lc"menu.options.graphics.ql.resolution.title"
    }

    elements += new Slider(PercentageSlider, SliderTextBox) {
      override def minValue: Double = 0.2
      override def maxValue: Double = 1.0

      override def setValue(newValue: Double): Unit = options.graphics.quality.resolutionFactor = newValue
      override def currentValue: Double = options.graphics.quality.resolutionFactor

      override def clamped: Boolean = true
      override def step: Option[Double] = Some(0.1)

      addTooltipToLayout(sliderInput, lastLayout, lc"menu.options.graphics.ql.resolution.desc")
      addTooltipToLayout(textBox.input, lastLayout, lc"menu.options.graphics.ql.resolution.desc")
    }

    elements
  }

  val volumeElements = {
    val elements = ArrayBuffer[Element]()
    val opt = options.audio

    elements += new Label(TitleLabel) {
      override def text: String = lc"menu.options.audio.vol.title"
    }


    def addVolumeSlider(name: String, getVolume: => Double, setVolume: Double => Unit): Unit = {
      elements += new Padding(10.0)

      elements += new Label(InfoLabel) {
        val text: String = lc"menu.options.audio.vol.$name.title"
      }

      elements += new Slider(PercentageSlider, SliderTextBox) {
        override def minValue: Double = 0.0
        override def maxValue: Double = 1.0

        override def setValue(newValue: Double): Unit = setVolume(newValue)
        override def currentValue: Double = getVolume

        override def clamped: Boolean = true
        override def step: Option[Double] = None

        addTooltipToLayout(sliderInput, lastLayout, lc"menu.options.audio.vol.$name.desc")
        addTooltipToLayout(textBox.input, lastLayout, lc"menu.options.audio.vol.$name.desc")
      }
    }

    addVolumeSlider("master", opt.volumeMaster, opt.volumeMaster_=)
    addVolumeSlider("sfx", opt.volumeSfx, opt.volumeSfx_=)
    addVolumeSlider("ui", opt.volumeUi, opt.volumeUi_=)
    addVolumeSlider("music", opt.volumeMusic, opt.volumeMusic_=)

    elements
  }

  val audioAdvancedElements = {
    val elements = ArrayBuffer[Element]()
    val opt = options.audio

    elements += new Label(TitleLabel) {
      override def text: String = lc"menu.options.audio.advanced.title"
    }

    elements += new Padding(10.0)

    elements += new Label(InfoLabel) {
      val text: String = lc"menu.options.audio.advanced.latency.title"
    }

    elements += new Slider(MillisecondSlider, SliderTextBox) {
      override def minValue: Double = 0.01
      override def maxValue: Double = 0.3

      override def setValue(newValue: Double): Unit = opt.latency = newValue
      override def currentValue: Double = opt.latency

      override def clamped: Boolean = true
      override def step: Option[Double] = None

      addTooltipToLayout(sliderInput, lastLayout, lc"menu.options.audio.advanced.latency.desc")
      addTooltipToLayout(textBox.input, lastLayout, lc"menu.options.audio.advanced.latency.desc")
    }

    elements += new Padding(10.0)

    elements += new Label(InfoLabel) {
      val text: String = lc"menu.options.audio.advanced.sampleRate.title"
    }

    elements += new LabelDropdown[Int](NormalDropdown, NormalLabel) {
      override val items: Seq[Int] = Array(8000, 22050, 44100, 48000, 96000)
      override def currentItem: Int = opt.sampleRate
      override def setItem(newItem: Int): Unit = opt.sampleRate = newItem
      override def itemToText(item: Int): String = item.toString

      addTooltipOption(inputOpen, if (!isOpen) Some(lc"menu.options.audio.advanced.sampleRate.desc") else None)
    }
  }

  val commonElements = {
    val elements = ArrayBuffer[Element]()
    val monitors = AppWindow.listMonitors

    elements += new Label(TitleLabel) {
      override def text: String = lc"menu.options.common.display"
    }

    elements += new Padding(10.0)

    elements += new Label(InfoLabel) {
      override def text: String = lc"menu.options.common.windowMode.title"
    }

    elements += new LabelDropdown[String](NormalDropdown, NormalLabel) {
      override val items: Seq[String] = Options.WindowModes
      override def currentItem: String = options.windowMode
      override def setItem(newItem: String): Unit = options.windowMode = newItem
      override def itemToText(item: String): String = lc"menu.options.common.windowMode.$item.name"

      addTooltipOption(inputOpen, if (!isOpen) Some(lc"menu.options.common.windowMode.desc") else None)

      addTooltipIndexed(inputSelect, index => {
        val item = Options.WindowModes(index)
        Locale.getSimpleOption(s"menu.options.common.windowMode.$item.desc")
      })
    }

    elements += new Padding(10.0)

    elements += new Label(InfoLabel) {
      override def text: String = lc"menu.options.common.monitor.title"
    }

    val primary = lc"menu.options.common.monitor.primary"
    val monitorNames = primary +: monitors.zipWithIndex.map({ case (m, i) =>
      val align = lc"menu.options.common.monitor.align.${m.align}"
      s"${i + 1}: $align"
    })

    elements += new LabelDropdown[Int](NormalDropdown, NormalLabel) {
      override val items: Seq[Int] = (0 until monitors.length + 1)
      override def currentItem: Int = options.monitor
      override def setItem(newItem: Int): Unit = options.monitor = newItem
      override def itemToText(item: Int): String = monitorNames(item)

      addTooltipOption(inputOpen, if (!isOpen) Some(lc"menu.options.common.monitor.desc") else None)

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

    elements += new Padding(30.0)

    elements += new Label(TitleLabel) {
      override val text: String = lc"menu.options.common.language"
    }

    elements += new Padding(10.0)

    elements += new Label(InfoLabel) {
      override val text: String = lc"menu.options.common.textLanguage.title"
    }

    elements += new LabelDropdown[LocaleInfo](NormalDropdown, NormalLabel) {
      override val items: Seq[LocaleInfo] = LocaleInfo.locales
      override def currentItem: LocaleInfo = {
        val langId = Identifier(options.language)
        LocaleInfo.locales.find(_.code == langId).getOrElse(LocaleInfo.defaultLocale)
      }
      override def setItem(newItem: LocaleInfo): Unit = options.language = newItem.code.toString
      override def itemToText(item: LocaleInfo): String = item.language

      addTooltipOption(inputOpen, if (!isOpen) Some(lc"menu.options.common.textLanguage.desc") else None)
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

  val allElements = commonElements ++ volumeElements ++ audioAdvancedElements ++ glElements ++ qualityElements ++ separateElements

  abstract class Tab {
    def name: String = ""
    def update(parent: Layout): Unit
  }

  object TabCommon extends Tab {
    override val name = lc"menu.options.common.tabName"

    override def update(parent: Layout): Unit = {
      val commonParent = parent.pushLeft(200.0)

      for (element <- commonElements) {
        element.update(commonParent)
      }
    }
  }

  object TabAudio extends Tab {
    override val name = lc"menu.options.audio.tabName"

    override def update(parent: Layout): Unit = {
      val volumeParent = parent.pushLeft(200.0)
      parent.padLeft(50.0)
      val advancedParent = parent.pushLeft(200.0)

      for (element <- volumeElements) {
        element.update(volumeParent)
      }

      for (element <- audioAdvancedElements) {
        element.update(advancedParent)
      }

    }

  }

  object TabRendering extends Tab {
    override val name = lc"menu.options.graphics.tabName"

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
  val tabs = Array(TabCommon, TabAudio, TabRendering)
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
        tooltip <- tooltips.get(focusedInput._1)
        text <- tooltip.textFunc(focusedInput._2)
      } {
        val layout = tooltip.overrideLayout.map(_()).getOrElse(focusedArea)
        val pos = Vector2(layout.x1, layout.y0) + unit *@ Vector2(15.0, 5.0)
        val width = unit.x * 200.0
        val bounds = Vector2(width, unit.y * 40000.0)
        val style = TooltipTextStyle.scaled(unit.y)
        val endY = canvas.drawTextWrapped(5, style, pos, bounds, text)
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
