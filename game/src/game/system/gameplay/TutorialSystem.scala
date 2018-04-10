package game.system.gameplay

import core._
import ui.{Canvas, Layout}
import TutorialSystem._
import game.options.Options
import locale.Locale
import locale.LocaleString._
import ui.Canvas.{Outline, TextStyle}
import asset._
import core.Vector2

object TutorialSystem {

  val TutorialFont = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")
  val TutorialTextStyle = TextStyle(TutorialFont, 22.0, outline = Outline(1.0))

  val MoveCamera = 0
  val MoveBoost = 1
  val MoveZoom = 2

  val Assets = new AssetBundle("TutorialSystem",
    TutorialFont,
  )

}

sealed trait TutorialSystem {

  /** Advance a tutorial stage */
  def progress(phase: Int, amount: Double): Unit

  /** Update the tutorial overlay */
  def update(dt: Double): Unit

  /** Render the tutorial overlay */
  def render(canvas: Canvas): Unit

}

final class TutorialSystemImpl extends TutorialSystem {

  var currentProgress: Double = 0.0
  var currentPhase: Int = MoveCamera
  var nextPhase: Int = MoveCamera
  var fade = 0.0

  override def progress(phase: Int, amount: Double): Unit = {
    if (phase == currentPhase) {
      currentProgress += amount
      if (currentProgress >= 1.0) {
        currentProgress = 0.0

        if (currentPhase == MoveCamera) {
          nextPhase = MoveBoost
        } else if (currentPhase == MoveBoost) {
            nextPhase = MoveZoom
        } else {
          nextPhase = -1
        }

      }
    }
  }

  override def update(dt: Double): Unit = {
    if (currentPhase == nextPhase) {
      fade += dt * 2.0
    } else {
      fade -= dt * 2.0
      if (fade <= 0.0) {
        currentPhase = nextPhase
      }
    }
    fade = clamp(fade, 0.0, 1.0)
  }

  def bindToText(bind: String): String = Locale.getSimpleOption(s"key.$bind").getOrElse(bind)

  override def render(canvas: Canvas): Unit = {
    val binds = Options.current.binds
    val area = Layout.screen720p.padAround(100.0).containSnapped(600.0, 100.0,
      snapScale = 2.0, magScale = 4.0, minScale = 8.0, anchor = Vector2(0.5, 1.0))

    val alpha = smoothStep(fade)
    val textStyle = TutorialTextStyle.copy(color = TutorialTextStyle.color.copy(a = alpha),
      outline = TutorialTextStyle.outline.copy(color = TutorialTextStyle.outline.color.copy(a = alpha)))

    val text = if (currentPhase == MoveCamera) {
      val keys = Vector(binds.cameraUp, binds.cameraLeft, binds.cameraDown, binds.cameraRight)
      val keyText = keys.map(bindToText).mkString(", ")
      Locale.getExpression("tutorial.moveCamera", "keys" -> keyText)
    } else if (currentPhase == MoveBoost) {
      val keyText = bindToText(binds.cameraBoost)
      Locale.getExpression("tutorial.boostCamera", "keys" -> keyText)
    } else if (currentPhase == MoveZoom) {
      lc"tutorial.zoomCamera"
    } else {
      ""
    }

    if (text.nonEmpty)
      canvas.drawTextWrapped(0, textStyle, area, text)
  }

}

