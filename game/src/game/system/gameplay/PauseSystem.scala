package game.system.gameplay

import core._
import ui._
import ui.Canvas._
import asset._
import locale._
import PauseSystem._
import PauseSystemImpl._
import game.options.Options
import game.state.GameState
import platform.{AppWindow, KeyEvent}

object PauseSystem {

  val Assets = new AssetBundle("TutorialSystem",
    InfoFont,
  )

}

sealed trait PauseSystem {

  /**
    * Is the game currently in a build phase.
    */
  def paused: Boolean

  /**
    * Update pause status.
    */
  def update(dt: Double): Unit

  /**
    * Render pause GUI.
    */
  def renderGui(canvas: Canvas, inputs: InputSet): Unit

}

object PauseSystemImpl {

  val InfoFont = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")
  val InfoText = TextStyle(InfoFont, 30.0, outline = Outline(1.0), align = AlignCenter)

}

final class PauseSystemImpl extends PauseSystem {

  var _paused: Boolean = true

  val unpauseBind = KeyEvent.NameToKey.get(Options.current.binds.unpause).getOrElse(KeyEvent.Space)
  val unpauseName = KeyEvent.KeyToName.get(unpauseBind).getOrElse("Space")

  override def paused: Boolean = _paused

  def bindToText(bind: String): String = Locale.getSimpleOption(s"key.$bind").getOrElse(bind)

  override def update(dt: Double): Unit = {

    if (enemySystem.numEnemiesActive <= 0) {
      if (_paused == false) {
        saveStateSystem.recreateMissingEntities()
      }

      _paused = true
    }

    if (AppWindow.keyDownEvents.exists(_.key == unpauseBind) && paused) {
      _paused = false
      enemySpawnSystem.spawnNextRound()
    }

  }

  override def renderGui(canvas: Canvas, inputs: InputSet): Unit = {
    val top = Layout.screen720p.padAround(50.0).pushTop(30.0)

    if (paused) {

      val keyText = bindToText(unpauseName)
      val text = Locale.getExpression("game.buildHint", "key" -> keyText)

      canvas.drawText(1, InfoText, top, text)
    } else {

      val num = enemySystem.numEnemiesActive
      val locale = if (num > 1) "game.playHint.many" else "game.playHint.one"
      val text = Locale.getExpression(locale, "num" -> num.toString)

      canvas.drawText(1, InfoText, top, text)
    }

  }
}


