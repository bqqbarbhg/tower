package menu

import core._
import ui._
import asset._
import PauseMenu._
import asset.AssetBundle
import ui.Canvas._
import locale.LocaleString._
import ui.InputSet.InputArea

object PauseMenu {

  val MenuAtlas = AtlasAsset("atlas/menu.s2at")
  val ButtonFont = FontAsset("font/catamaran/Catamaran-SemiBold.ttf.s2ft")

  val BackgroundSprite = Identifier("gui/menu/pause_fade.png")
  val ButtonTextStyle = TextStyle(ButtonFont, 20.0, align = AlignCenter)

  val Width = 200.0
  val Height = 200.0
  val FadeSpeed = 5.0

  val Assets = new AssetBundle("PauseMenu",
    MenuAtlas
  )

  class Button(val localeKey: String) {
    val text = lc"menu.pausemenu.button.$localeKey"
    val input = new InputArea()
  }
}

class PauseMenu(val inputs: InputSet, val canvas: Canvas) {

  var isOpen: Boolean = false
  var fade: Double = 0.0

  val ContinueButton = new Button("continue")
  val ReturnToMenuButton = new Button("returnToMenu")

  def update(dt: Double): Unit = {

    if (isOpen) {
      fade += dt * FadeSpeed
    } else {
      fade -= dt * FadeSpeed
    }

    fade = clamp(fade, 0.0, 1.0)

    val area = Layout.screen720p.padAround(400.0).containSnapped(Width, Height,
      snapScale = 1.0, magScale = 2.0, minScale = 8.0)

    val buttonsArea = area.copy.padTop(20.0).padBottom(20.0)
    val buttons = Seq(ContinueButton, ReturnToMenuButton)


    if (fade > 0.001) {
      val sf = smoothStep(fade)

      val color = Color(0.5, 0.5, 0.5, sf)

      val rect = area.copy
      val pad = (1.0 - sf) * rect.unit.x * 100.0
      rect.x0 -= pad
      rect.x1 += pad

      canvas.draw(0, BackgroundSprite, rect, color)

      buttonsArea.padTop(40.0)
      for (button <- buttons) {
        val buttonArea = buttonsArea.pushTop(30.0)
        val alpha = sf * (if (button.input.focused) 0.75 else 0.5)
        val style = ButtonTextStyle.copy(color = ButtonTextStyle.color.copy(a = alpha))
        canvas.drawText(0, style, buttonArea, button.text)
        inputs.add(1, button.input, buttonArea)
      }
    }

  }

}

