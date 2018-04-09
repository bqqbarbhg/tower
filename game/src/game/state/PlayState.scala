package game.state

import java.io.FileOutputStream

import core._
import render._
import ui._
import asset._
import platform.{AppWindow, KeyEvent}
import game.system
import game.system.base._
import game.system.rendering._
import game.system.gameplay._
import game.system.audio._
import game.shader._
import menu.{DebugMenu, PauseMenu}
import PlayState._
import game.options.Options
import game.system.EntitySet
import game.system.audio.AudioSystem.SoundRef
import game.system.rendering.AmbientSystem.Probe
import io.property._
import io.serialization.{BinaryReader, BinaryWriter}
import menu.gui.TextBox
import task.{Scheduler, Task}
import util.geometry.Frustum
import util.BufferUtils._

import scala.collection.mutable.ArrayBuffer

object PlayState {

  val GroundTexture = TextureAsset("game/ground/ground_albedo.png.s2tx")
  val Colorgrade = TextureAsset("colorgrade/ingame.png.s2tx")

  val IdleMusic = SoundAsset("audio/music/ingame.ogg.s2au")

  val Assets = new AssetBundle("PlayState",
    PauseMenu.Assets,
    TutorialSystem.Assets,
    GroundTexture,
    Colorgrade,
  )
}

class PlayState(val loadExisting: Boolean) extends GameState {

  val inputs = new InputSet()
  val canvas = new Canvas()
  val pauseMenu = new PauseMenu(inputs, canvas)
  var prevTime = 0.0
  var finished: Boolean = false

  var music: SoundRef = null

  override def load(): Unit = {
    Assets.acquire()
    GameState.push(new LoadingState())
  }

  override def start(): Unit = {

    system.base.loadState()
    system.rendering.loadState()
    system.rendering.loadGame()
    system.gameplay.loadGame()

    prevTime = AppWindow.currentTime

    music = audioSystem.play(IdleMusic, AudioSystem.Music)
    music.instance.setFullLoop()

    if (loadExisting)
      loadGame()
  }

  override def stop(): Unit = {
    music.stop()
    audioSystem.update()

    saveGame()

    entitySystem.deleteAllEntities()

    system.gameplay.unloadGame()
    system.rendering.unloadGame()
    system.rendering.unloadState()
    system.base.unloadState()

    Assets.release()
  }

  // -- Persistency

  def saveGame(): Unit = {
    val buffer = Memory.alloc(1024 * 16)
    val header = Memory.alloc(1024)

    val writer = new BinaryWriter(buffer)
    writer.write(Camera)

    writer.writeHeader(header)

    header.finish()
    buffer.finish()

    val os = new FileOutputStream("save.s2bi")
    header.writeTo(os)
    buffer.writeTo(os)
    os.close()

    Memory.free(buffer)
    Memory.free(header)
  }

  def loadGame(): Unit = {
    val file = new java.io.File("save.s2bi")
    if (file.exists && file.canRead) {
      val buffer = Memory.alloc(1024 * 16)
      buffer.readFromFile("save.s2bi")
      buffer.finish()

      val reader = new BinaryReader(buffer)

      reader.read(Camera)

      Memory.free(buffer)
    }
  }

  // -- Pause

  def isPaused: Boolean = pauseMenu.isOpen

  def updatePauseMenu(dt: Double): Unit = {

    def processEscape(): Unit = {
      if (debugMenu.isDefined) {
        debugUnselect()
        return
      }

      pauseMenu.isOpen = !pauseMenu.isOpen
    }

    for (e <- AppWindow.keyDownEvents) {
      if (e.key == KeyEvent.Escape) {
        processEscape()
      }
    }

    if (pauseMenu.ContinueButton.input.clicked) {
      pauseMenu.isOpen = false
    }

    if (pauseMenu.ReturnToMenuButton.input.clicked) {
      finished = true
      GameState.push(new MenuState())
    }

    pauseMenu.update(dt)
  }

  // -- Debug menu

  object DebugSelector extends PropertyContainer {
    private val arr = MacroPropertySet.make[DebugSelector.type]()
    override val propertySet: PropertySet = new PropertySet("DebugSelector", arr)

    var name: StringProp.Type = ""
  }

  var debugMenu: Option[DebugMenu] = None

  def updateDebugMenu(): Unit = {

    if (AppWindow.keyDownEvents.exists(e => e.key == 'G' && e.control)) {
      debugOpenSelector()
    }

    for (debug <- debugMenu) {
      val area = Layout.screen.padAround(10.0).pushLeft(300.0)
      debug.update(area)

      if (debug.propObj eq DebugSelector) {
        if (AppWindow.keyDownEvents.exists(_.key == KeyEvent.Enter)) {
          debugObjectByName.lift.apply(DebugSelector.name.toLowerCase) match {
            case Some(obj) => debugSelect(obj)
            case None => debugUnselect()
          }
        }
      }
    }
  }

  def debugOpenSelector(): Unit = {
    DebugSelector.name = ""
    debugSelect(DebugSelector)
  }

  val debugObjectByName: PartialFunction[String, PropertyContainer] = {
    case "camera" => Camera
    case "cameratweak" => CameraTweak
  }

  def debugSelect(target: PropertyContainer): Unit = {
    debugMenu = Some(new DebugMenu(inputs, canvas, target))
  }

  def debugUnselect(): Unit = {
    debugMenu = None
  }

  // -- Camera movement

  def keyInputEnabled: Boolean = {
    if (debugMenu.exists(_.elements.collect({ case t: TextBox => t}).exists(_.editContext.nonEmpty))) {
      return false
    }

    true
  }

  object CameraBind {
    val binds = Options.current.binds

    val Up = KeyEvent.NameToKey.get(binds.cameraUp).getOrElse('W'.toInt)
    val Down = KeyEvent.NameToKey.get(binds.cameraDown).getOrElse('S'.toInt)
    val Left = KeyEvent.NameToKey.get(binds.cameraLeft).getOrElse('A'.toInt)
    val Right = KeyEvent.NameToKey.get(binds.cameraRight).getOrElse('D'.toInt)
    val Boost = KeyEvent.NameToKey.get(binds.cameraBoost).getOrElse(KeyEvent.LeftShift)

  }

  def updateCameraMovement(dt: Double): Unit = {

    var move = Vector2.Zero

    if (keyInputEnabled) {
      if (AppWindow.keyDown(CameraBind.Up))    move += Vector2(0.0, -1.0)
      if (AppWindow.keyDown(CameraBind.Down))  move += Vector2(0.0, +1.0)
      if (AppWindow.keyDown(CameraBind.Left))  move += Vector2(-1.0, 0.0)
      if (AppWindow.keyDown(CameraBind.Right)) move += Vector2(+1.0, 0.0)
    }

    val len = move.length
    if (len > 0.0) {
      tutorialSystem.progress(TutorialSystem.MoveCamera, dt)

      move /= len

      val boost = if (AppWindow.keyDown(CameraBind.Boost)) {
        tutorialSystem.progress(TutorialSystem.MoveBoost, dt)
        CameraTweak.moveBoostAmount
      } else {
        1.0
      }

      cameraVel *= math.pow(0.5, dt / (1.0 / 60.0))
      cameraVel += move *@ CameraTweak.moveSpeed * boost * dt
    } else {
      cameraVel *= math.pow(0.8, dt / (1.0 / 60.0))
    }

    Camera.position += cameraVel * dt
  }

  // -- Scene render

  object Camera extends PropertyContainer {
    private val arr = MacroPropertySet.make[Camera.type]()
    override val propertySet: PropertySet = new PropertySet("Camera", arr)

    var position: Vector2Prop.Type = Vector2.Zero
    var zoom: DoubleProp.Type = 20.0
  }

  object CameraTweak extends PropertyContainer {
    private val arr = MacroPropertySet.make[CameraTweak.type]()
    override val propertySet: PropertySet = new PropertySet("CameraTweak", arr)

    var fov: DoubleProp.Type = 90.0
    var moveSpeed: Vector2Prop.Type = Vector2(400.0, 500.0)
    var moveBoostAmount: DoubleProp.Type = 2.0
  }

  var cameraPos = Vector3.Zero
  var cameraVel = Vector2.Zero

  var view = Matrix43.Identity
  var projection = Matrix4.Identity
  var viewProjection = Matrix4.Identity

  val visibleEntities = new EntitySet()

  object DepEntities
  object DepProbes
  object DepMeshes
  object DepCables
  object DepGround

  def renderScene(): Unit = {

    val frustum = Frustum.fromViewProjection(viewProjection)

    var visibleProbes: ArrayBuffer[Probe] = null
    var visibleMeshes: ModelSystem.MeshInstanceCollection = null
    var visibleCables: ArrayBuffer[CableRenderSystem.CableMeshPart] = null
    var visibleGround: ArrayBuffer[GroundSystem.GroundPlate] = null
    var forwardDraws: ForwardRenderingSystem.Draws = null

    visibleEntities.clear()

    val s = new Scheduler()

    s.add("Viewport cull")(cullingSystem, DepEntities)() {
      cullingSystem.cullEntities(visibleEntities, frustum, CullingSystem.MaskRender)
    }

    s.add("Probe collect")(ambientSystem, DepProbes)(DepEntities) {
      visibleProbes = ambientSystem.updateVisibleProbes(visibleEntities)
    }

    s.add("Ambient point dynamic")(ambientPointLightSystem)() {
      ambientPointLightSystem.updateDynamicLights()
    }

    s.add("Ambient probe points")(DepProbes)(ambientPointLightSystem) {
      ambientPointLightSystem.updateVisibleProbes(visibleProbes)
    }

    s.add("Ambient probe indirect")(ambientSystem, DepProbes)() {
      ambientSystem.updateIndirectLight(visibleProbes)
    }

    s.add("Model update")(modelSystem, DepMeshes)(DepEntities) {
      val models = modelSystem.collectVisibleModels(visibleEntities)
      modelSystem.updateModels(models)
      visibleMeshes = modelSystem.collectMeshInstances(models)
    }

    s.add("Collect cables")(cableRenderSystem, DepCables)(DepEntities) {
      visibleCables = cableRenderSystem.collectCableMeshes(visibleEntities)
    }

    s.add("Collect ground")(groundSystem, DepGround)(DepEntities) {
      visibleGround = groundSystem.collectGroundPlates(visibleEntities)
    }

    s.addTo("Forward draws")(Task.Main)(forwardRenderingSystem)(DepMeshes, DepProbes) {
      forwardDraws = forwardRenderingSystem.createMeshDraws(visibleMeshes)
    }

    s.add("Ambient point cleanup")(ambientPointLightSystem)() {
      ambientPointLightSystem.frameCleanup()
    }

    s.add("Ambient cleanup")(ambientSystem, DepProbes)() {
      ambientSystem.frameCleanup(visibleProbes)
    }

    s.add("Model cleanup")(modelSystem)() {
      modelSystem.frameCleanup()
    }

    s.finish()

    val renderer = Renderer.get

    renderer.setWriteSrgb(false)
    renderer.setRenderTarget(globalRenderSystem.mainTargetMsaa)
    renderer.clear(Some(Color.Black), None)

    renderer.pushUniform(GlobalSceneUniform, u => {
      GlobalSceneUniform.ViewProjection.set(u, viewProjection)
    })

    renderer.setTexture(GroundShader.Textures.Albedo, GroundTexture.get.texture)

    GroundShader.get.use()

    for (ground <- visibleGround) {
      ground.draw()
    }

  }

  override def update(): Unit = {
    AppWindow.pollEvents()

    val time = AppWindow.currentTime
    val dt = time - prevTime
    prevTime = time

    inputs.update()

    updatePauseMenu(dt)
    updateDebugMenu()

    if (!isPaused) {
      tutorialSystem.update(dt)
      updateCameraMovement(dt)
    }

    audioSystem.update()

    tutorialSystem.render(canvas)

    val renderer = Renderer.get

    renderer.beginFrame()

    val screenWidth = globalRenderSystem.screenWidth
    val screenHeight = globalRenderSystem.screenWidth
    val aspectRatio = screenWidth.toDouble / screenHeight.toDouble

    val cameraPos = Vector3(Camera.position.x, Camera.zoom, Camera.position.y)
    val fov = math.toRadians(CameraTweak.fov)

    view = Matrix43.look(cameraPos, Vector3(0.0, -5.0, -1.0))
    projection = Matrix4.perspective(aspectRatio, fov, 1.0, 200.0)
    viewProjection = projection * view

    if (globalRenderSystem.renderingEnabled) {
      renderScene()
    }

    renderer.setDepthMode(false, false)
    renderer.setCull(false)
    renderer.setWriteSrgb(false)
    renderer.setBlend(Renderer.BlendNone)
    renderer.setRenderTarget(globalRenderSystem.msaaResolveTarget)

    TonemapShader.get.use()

    if (globalRenderSystem.msaa > 1)
      renderer.setTextureTargetColor(TonemapShader.Textures.BackbufferMsaa, globalRenderSystem.mainTargetMsaa, 0)
    else
      renderer.setTextureTargetColor(TonemapShader.Textures.Backbuffer, globalRenderSystem.mainTargetMsaa, 0)

    renderer.setTexture(TonemapShader.Textures.ColorLookup, Colorgrade.get.texture)

    renderer.drawQuad()

    renderer.setRenderTarget(RenderTarget.Backbuffer)
    renderer.clear(Some(Color.Black), None)

    renderer.setWriteSrgb(false)
    PostprocessShader.get.use()

    renderer.setTextureTargetColor(TonemapShader.Textures.Backbuffer, globalRenderSystem.msaaResolveTarget, 0)

    renderer.drawQuad()

    renderer.setWriteSrgb(true)
    canvas.render()

    LayoutDebugger.render()
    DebugDraw.render2D()

    renderer.endFrame()

    AppWindow.swapBuffers()
  }

  override def done: Boolean = finished

}

