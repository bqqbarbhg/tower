package game.menu

import core._
import ui._
import asset._
import HotbarMenu._
import game.options.Options
import locale.Locale
import platform.{AppWindow, KeyEvent}
import ui.Canvas.TextStyle
import ui.InputSet.InputArea

object HotbarMenu {

  val HotkeyFont = FontAsset("font/catamaran/Catamaran-SemiBold.ttf.s2ft")

  val HotkeyStyleActive = TextStyle(HotkeyFont, 20.0, color = Color.rgba(0xFFFFFF, 0.9))
  val HotkeyStyleInactive = TextStyle(HotkeyFont, 20.0, color = Color.rgba(0xFFFFFF, 0.3))

  val MenuAtlas = AtlasAsset("atlas/menu.s2at")
  val BarAtlas = AtlasAsset("atlas/bar.s2at")

  val Assets = new AssetBundle("HotbarMenu",
    BarAtlas,
    MenuAtlas,
    HotkeyFont,
  )

  val Quad = Identifier("gui/menu/background_white.png")

  val PartPadding = 10.0
  val BottomPadding = 10.0
  val HotkeyLeftPadding = 5.0

  val IconSize = 50.0
  val IconPad = 5.0
  val CellSize = IconSize + IconPad * 2.0

  class Category(val icon: Identifier, val bind: Int) {
    val bindName = KeyEvent.KeyToName.get(bind).flatMap(b => Locale.getSimpleOption(s"key.$b")).getOrElse("")

    val input = new InputArea()
  }

  val IconOpenColor = Color.rgba(0xFFFFFF, 0.9)
  val IconFocusColor = Color.rgba(0xFFFFFF, 0.7)
  val IconFocusInactiveColor = Color.rgba(0xFFFFFF, 0.5)
  val IconIdleColor = Color.rgba(0xFFFFFF, 0.5)
  val IconInactiveColor = Color.rgba(0xFFFFFF, 0.3)

  val CatBgColor = Color.rgba(0x000000, 0.4)

}

class HotbarMenu(val inputs: InputSet, val canvas: Canvas) {

  var prevRightClick = false
  val binds = Options.current.binds

  val hotkeys = Vector(
    KeyEvent.NameToKey.get(binds.bar1).getOrElse('1'.toInt),
    KeyEvent.NameToKey.get(binds.bar2).getOrElse('2'.toInt),
    KeyEvent.NameToKey.get(binds.bar3).getOrElse('3'.toInt),
    KeyEvent.NameToKey.get(binds.bar4).getOrElse('4'.toInt),
    KeyEvent.NameToKey.get(binds.bar5).getOrElse('5'.toInt),
  )

  val categories = Vector(
    new Category(Identifier("gui/bar/icon_turret.png"), hotkeys(0)),
    new Category(Identifier("gui/bar/icon_radar.png"), hotkeys(1)),
    new Category(Identifier("gui/bar/icon_cable.png"), hotkeys(2)),
    new Category(Identifier("gui/bar/icon_structure.png"), hotkeys(3)),
  )

  var openCategory: Option[Category] = None

  def update(dt: Double): Unit = {
    val area = Layout.screen.containSnapped(CellSize * 10.0, CellSize * 2.0 + PartPadding + BottomPadding,
      snapScale = 1.0,
      magScale = 2.0,
      minScale = 4.0,
      anchor = Vector2(0.5, 1.0),
      relativeScale = 0.5,
    )

    for (cat <- categories) {
      if (cat.input.clicked) {
        openCategory = Some(cat)
      }
    }

    openCategory match {
      case Some(cat) =>
      case None =>
        for (key <- AppWindow.keyDownEvents) {
          for (cat <- categories) {
            if (key.key == cat.bind) {
              openCategory = Some(cat)
            }
          }
        }
    }

    val rightClick = AppWindow.mouseButtonDown(1)
    if (rightClick && !prevRightClick) {
      openCategory = None
    }
    prevRightClick = rightClick

    val fullArea = area.copy
    val top = area.pushTop(CellSize)
    area.padTop(PartPadding)
    val bottom = area.pushTop(CellSize)

    canvas.draw(0, Quad, bottom, CatBgColor)

    val mutBottom = bottom.copy
    for (cat <- categories) {
      val button = mutBottom.pushLeft(CellSize)
      val iconPad = button.copy.padAround(IconPad)

      inputs.add(0, cat.input, button)

      if (openCategory.contains(cat)) {
        val bridgeArea = button.edgeTop.extendTop(BottomPadding)
        canvas.draw(0, Quad, bridgeArea, CatBgColor)
        canvas.draw(0, Quad, top, CatBgColor)
      }

      val hotkeyStyle = if (openCategory.isEmpty) HotkeyStyleActive else HotkeyStyleInactive
      val hotkeyArea = button.copy.pushTop(hotkeyStyle.height).padLeft(HotkeyLeftPadding)
      canvas.drawText(1, hotkeyStyle, hotkeyArea, cat.bindName)

      val color = if (openCategory.contains(cat)) IconOpenColor
      else if (cat.input.focused && openCategory.nonEmpty) IconFocusInactiveColor
      else if (cat.input.focused) IconFocusColor
      else if (openCategory.nonEmpty) IconInactiveColor
      else IconIdleColor

      canvas.draw(1, cat.icon, iconPad, color)
    }

  }

}

