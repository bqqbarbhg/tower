package game.system.gameplay

import core._
import ui.{Canvas, InputSet, Layout}
import TutorialSystem._
import TutorialSystemImpl._
import game.options.Options
import locale.Locale
import locale.LocaleString._
import ui.Canvas._
import asset._
import com.sun.deploy.panel.DeleteFilesDialog
import core.Vector2
import game.system.gameplay.TutorialSystemImpl.PersistentState
import io.property._
import ui.InputSet.InputArea

object TutorialSystem {

  val TutorialFont = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")
  val TitleFont = FontAsset("font/catamaran/Catamaran-SemiBold.ttf.s2ft")
  val TitleTextStyle = TextStyle(TitleFont, 22.0, align = AlignCenter, outline = Outline(1.0))
  val TutorialTextStyle = TextStyle(TutorialFont, 16.0, outline = Outline(1.0))

  val MoveCamera = 0
  val MoveBoost = 1
  val MoveZoom = 2
  val SelectTurretCategory = 3
  val SelectTurret = 4
  val BuildTurret = 5
  val EndBuildTurret = 6
  val SelectRadar = 7
  val BuildRadar = 8
  val EndBuildRadar = 9
  val StartBuildCable = 10
  val EndBuildCable = 11
  val StartRound = 12
  val WaitRound = 13
  val RemoveTurret = 14
  val SelectDirectedTurret = 15
  val BuildDirectedTurret = 16
  val EndBuildDirectedTurret = 17
  val ConnectDirectedTurret = 18
  val StartSecondRound = 19

  // Special "messages"
  val BuildAny = 1000
  val BuildNoSelection = 1001
  val BuildNoCable = 1002
  val AnyRoundStart = 1003
  val AnyRoundEnd = 1004
  val BuildAnyDelete = 1005
  val BuildAnyCable = 1006

  val Assets = new AssetBundle("TutorialSystem",
    MenuAtlas,
    TutorialFont,
    TitleFont,
  )

}

sealed trait TutorialSystem {

  /** Start doing the tutorial for this game */
  def startTutorial(): Unit

  /** Advance a tutorial stage */
  def progress(phase: Int, amount: Double): Unit

  /** Update the tutorial overlay */
  def update(dt: Double): Unit

  /** Render the tutorial overlay */
  def render(canvas: Canvas, inputs: InputSet): Unit

  /** Peristent data to save/load */
  def persistentState: PropertyContainer

  /** If true PauseSystem is allowed to start the next round */
  def allowStartRound: Boolean

  /** Substring that must be contained in the name allowed buildables */
  def allowedBuilding: Option[String]

  /** Substring that must be contained in the name allowed cable targets */
  def allowedCableTarget: Option[String]

  /** Allow deleting buildings */
  def allowedDeleteTarget: Option[String]

  /** Allowed build rotation */
  def allowedRotationIndex: Option[Int]

  /** Allow rotating buildings */
  def allowRotate: Boolean

  /** Allowed camera area */
  def cameraArea: Option[(Vector2, Vector2)]

  /** Allowed build area */
  def buildArea: Option[(Vector2, Vector2)]

}

object TutorialSystemImpl {

  object PersistentState {
    private val arr = MacroPropertySet.make[PersistentState]()
    private val propertySet: PropertySet = new PropertySet("TutorialSystem.PersistentState", arr)
  }

  class PersistentState extends PropertyContainer {
    override def propertySet: PropertySet = PersistentState.propertySet

    var currentProgress: DoubleProp.Type = 0.0
    var currentPhase: IntProp.Type = -1
    var nextPhase: IntProp.Type = -1
  }

  /** Buildable filter that doesn't match anything */
  val NoMatchingEntity = Some("@@@")

  val MenuAtlas = AtlasAsset("atlas/menu.s2at")
  val QuadSprite = Identifier("gui/menu/background_white.png")

}

final class TutorialSystemImpl extends TutorialSystem {

  val ps = new PersistentState()

  var fade = 0.0

  override def persistentState = ps

  override def startTutorial(): Unit = {
    ps.nextPhase = MoveCamera
  }

  override def progress(phase: Int, amount: Double): Unit = {

    val cur = ps.currentPhase
    val phaseMatch = if (cur == ps.nextPhase) {
      phase match {
        case BuildAny => cur == BuildTurret || cur == BuildRadar || cur == BuildDirectedTurret
        case BuildNoSelection => cur == EndBuildTurret || cur == EndBuildRadar || cur == EndBuildDirectedTurret
        case AnyRoundStart => cur == StartRound || cur == StartSecondRound
        case AnyRoundEnd => cur == WaitRound
        case BuildAnyDelete => cur == RemoveTurret
        case BuildAnyCable => cur == EndBuildCable || cur == ConnectDirectedTurret
        case any => any == ps.currentPhase
      }
    } else {
      false
    }

    if (phaseMatch) {
      ps.currentProgress += amount
      if (ps.currentProgress >= 0.999) {
        ps.currentProgress = 0.0

        if (ps.currentPhase >= MoveCamera && ps.currentPhase < StartSecondRound) {
          ps.nextPhase = ps.currentPhase + 1
        } else {
          ps.nextPhase = -1
        }
      }
    } else phase match {
      case BuildNoCable =>
        if (cur == EndBuildCable && ps.nextPhase == EndBuildCable)
          ps.nextPhase = StartBuildCable

      case _ => // No action
    }
  }

  override def update(dt: Double): Unit = {

    if (ps.currentPhase == ps.nextPhase) {
      fade += dt * 2.0
    } else {
      fade -= dt * 2.0
      if (fade <= 0.0) {
        ps.currentPhase = ps.nextPhase
      }
    }
    fade = clamp(fade, 0.0, 1.0)
  }

  def bindToText(bind: String): String = Locale.getSimpleOption(s"key.$bind").getOrElse(bind)

  override def render(canvas: Canvas, inputs: InputSet): Unit = {
    if (ps.currentPhase < 0) return

    val binds = Options.current.binds
    val area = Layout.screen720p.padAround(30.0).containSnapped(200.0, 200.0,
      snapScale = 2.0, magScale = 4.0, minScale = 8.0, anchor = Vector2(0.0, 0.7), relativeScale = 0.5)

    val alpha = smoothStep(fade)
    val textStyle = TutorialTextStyle.copy(color = TutorialTextStyle.color.copy(a = alpha),
      outline = TutorialTextStyle.outline.copy(color = TutorialTextStyle.outline.color.copy(a = alpha)))

    {
      val labelArea = area.pushTop(20.0)

      canvas.drawText(0, TitleTextStyle, labelArea, lc"tutorial.infoLabel")
      area.padTop(10.0)
    }

    val phase = ps.currentPhase
    val text = if (phase == MoveCamera) {
      val keys = Vector(binds.cameraUp, binds.cameraLeft, binds.cameraDown, binds.cameraRight)
      val keyText = keys.map(bindToText).mkString(", ")
      Locale.getExpression("tutorial.moveCamera", "keys" -> keyText)
    } else if (phase == MoveBoost) {
      val keyText = bindToText(binds.cameraBoost)
      Locale.getExpression("tutorial.boostCamera", "keys" -> keyText)
    } else if (phase == MoveZoom) {
      lc"tutorial.zoomCamera"
    } else if (phase == SelectTurretCategory) {
      lc"tutorial.selectTurretCategory"
    } else if (phase == SelectTurret) {
      lc"tutorial.selectTurret"
    } else if (phase == BuildTurret) {
      lc"tutorial.buildTurret"
    } else if (phase == EndBuildTurret) {
      lc"tutorial.endBuild"
    } else if (phase == SelectRadar) {
      lc"tutorial.selectRadar"
    } else if (phase == BuildRadar) {
      lc"tutorial.buildRadar"
    } else if (phase == EndBuildRadar) {
      lc"tutorial.endBuild"
    } else if (phase == StartBuildCable) {
      lc"tutorial.startBuildCable"
    } else if (phase == EndBuildCable) {
      lc"tutorial.endBuildCable"
    } else if (phase == StartRound) {
      val keyText = bindToText(binds.unpause)
      Locale.getExpression("tutorial.startRound", "key" -> keyText)
    } else if (phase == WaitRound) {
      lc"tutorial.waitRound"
    } else if (phase == RemoveTurret) {
      val keyText = bindToText(binds.delete)
      Locale.getExpression("tutorial.removeTurret", "key" -> keyText)
    } else if (phase == SelectDirectedTurret) {
      lc"tutorial.selectDirectedTurret"
    } else if (phase == BuildDirectedTurret) {
      val keyText = bindToText(binds.rotate)
      Locale.getExpression("tutorial.buildDirectedTurret", "key" -> keyText)
    } else if (phase == EndBuildDirectedTurret) {
      lc"tutorial.endBuild"
    } else if (phase == ConnectDirectedTurret) {
      lc"tutorial.connectDirectedTurret"
    } else if (phase == StartSecondRound) {
      val keyText = bindToText(binds.unpause)
      Locale.getExpression("tutorial.startRound", "key" -> keyText)
    } else {
      ""
    }

    if (text.nonEmpty)
      canvas.drawTextWrapped(0, textStyle, area, text)
  }

  override def allowStartRound: Boolean = ps.nextPhase == -1 || ps.currentPhase == StartRound || ps.currentPhase == StartSecondRound

  override def allowedBuilding: Option[String] = {
    if (ps.nextPhase == -1) return None

    val phase = ps.nextPhase
    if (phase == ps.currentPhase) {

      if (phase == BuildTurret) {
        Some("turret_basic")
      } else if (phase == BuildRadar) {
        Some("radar_tutorial")
      } else if (phase == BuildDirectedTurret) {
        Some("turret_directed")
      } else {
        NoMatchingEntity
      }

    } else {
      NoMatchingEntity
    }
  }

  override def allowedCableTarget: Option[String] = {
    if (ps.nextPhase == -1) return None

    val phase = ps.nextPhase
    if (phase == ps.currentPhase) {

      if (phase == StartBuildCable) {
        Some("radar_tutorial")
      } else if (phase == EndBuildCable) {
        Some("turret_basic")
      } else if (phase == ConnectDirectedTurret) {
        Some("")
      } else {
        NoMatchingEntity
      }

    } else {
      NoMatchingEntity
    }
  }

  override def allowedDeleteTarget: Option[String] = {
    if (ps.nextPhase == -1) return None

    val phase = ps.nextPhase
    if (phase == ps.currentPhase) {

      if (phase == RemoveTurret) {
        Some("turret_basic")
      } else {
        NoMatchingEntity
      }

    } else {
      NoMatchingEntity
    }
  }

  override def allowRotate: Boolean = {
    if (ps.nextPhase == -1) return true

    if (ps.nextPhase >= BuildDirectedTurret)
      return true

    false
  }

  override def allowedRotationIndex: Option[Int] = {
    if (ps.nextPhase == -1) return None

    if (ps.nextPhase == BuildDirectedTurret) {
      Some(3)
    } else {
      None
    }
  }

  override def cameraArea: Option[(Vector2, Vector2)] = {
    if (ps.nextPhase == -1) return None
    if (ps.nextPhase <= MoveZoom) return None

    Some((Vector2(-25.0, 0.0), Vector2(25.0, 35.0)))
  }

  override def buildArea: Option[(Vector2, Vector2)] = {
    if (ps.nextPhase == -1) return None

    Some((Vector2(-9.0, -9.0), Vector2(9.0, 9.0)))
  }
}

