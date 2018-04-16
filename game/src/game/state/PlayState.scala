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
import game.component._
import game.menu.HotbarMenu
import game.options.Options
import game.system.{Entity, EntitySet, EntityType}
import game.system.audio.AudioSystem.SoundRef
import game.system.rendering.AmbientSystem.Probe
import game.system.rendering.ModelSystem.ModelInstance
import gfx.{AnimationState, Material}
import io.property._
import io.serialization.{BinaryReader, BinaryWriter}
import menu.gui.TextBox
import task.{Scheduler, Task}
import util.geometry.Frustum
import util.BufferUtils._
import render.Renderer._

import scala.collection.mutable.ArrayBuffer

object PlayState {

  val GroundTexture = TextureAsset("game/ground/ground_albedo.png.s2tx")
  val Colorgrade = TextureAsset("colorgrade/ingame.png.s2tx")

  val IdleMusic = SoundAsset("audio/music/ingame.ogg.s2au")

  val TestPlaceModel = ModelAsset("game/tower/turret_basic/tower_turret.fbx.s2md")
  val TestPlaceMask = TextureAsset("game/tower/turret_basic/placemask.png.s2tx")
  val NoiseTex = TextureAsset("effect/noise.png.s2tx")

  val CablePulseMask = TextureAsset("effect/pulse/mask/basic.png.s2tx")
  val CablePulseTex = TextureAsset("effect/pulse/tex/hexagon.png.s2tx")

  val Assets = new AssetBundle("PlayState",
    PauseMenu.Assets,
    TutorialSystem.Assets,
    HotbarMenu.Assets,
    BuildSystem.Assets,

    GroundTexture,
    Colorgrade,
    GroundShader,
    InstancedMeshShader,
    SkinnedMeshShader,
    CableShader,
    CablePulseShader,
    IdleMusic,

    CablePulseMask,
    CablePulseTex,
  )
}

class PlayState(val loadExisting: Boolean) extends GameState {

  val inputs = new InputSet()
  val canvas = new Canvas()
  val pauseMenu = new PauseMenu(inputs, canvas)
  val hotbarMenu = new HotbarMenu(inputs, canvas)
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

    {
      val entity = entitySystem.create(EntityTypeAsset("entity/enemy/test.es.toml").get, Vector3(0.0, 0.0, -5.0))
      val model = modelSystem.collectModels(entity).head
      val anim = animationSystem.addAnimator(entity, model)
      anim.playLoop(0, Identifier("Move"))
    }

    {
      val entity = entitySystem.create(EntityTypeAsset("entity/enemy/test.es.toml").get, Vector3(0.0, 0.0, -16.0))
      val model = modelSystem.collectModels(entity).head
      val anim = animationSystem.addAnimator(entity, model)
      anim.playLoop(0, Identifier("Move"))
    }
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

      if (hotbarMenu.openCategory.nonEmpty) {
        hotbarMenu.openCategory = None
        hotbarMenu.selectedItem = None
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

    if (AppWindow.keyDownEvents.exists(e => e.key == 'H' && e.control)) {
      debugSelect(DebugView)
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
    case "debugview" => DebugView
    case "cable" => CableSystem.CableTweak
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

    val invertZoom = binds.invertScroll

  }

  def updateCameraMovement(dt: Double): Unit = {

    var zoomDelta = -AppWindow.mouseScroll
    if (CameraBind.invertZoom) zoomDelta = -zoomDelta

    if (math.abs(zoomDelta) >= 0.001) {
      tutorialSystem.progress(TutorialSystem.MoveZoom, math.abs(AppWindow.mouseScroll) * 0.2)
      zoomDelta = clamp(zoomDelta, -3.0, 3.0)
      Camera.zoom += zoomDelta * CameraTweak.zoomSpeed
      Camera.zoom = clamp(Camera.zoom, CameraTweak.minZoom, CameraTweak.maxZoom)
    }

    {
      val maxLinear = CameraTweak.zoomInterpolateLinear * dt
      Camera.interpolatedZoom += math.pow(CameraTweak.zoomInterpolateExponential, dt / (1.0 / 60.0)) * (Camera.zoom - Camera.interpolatedZoom)
      Camera.interpolatedZoom += clamp(Camera.zoom - Camera.interpolatedZoom, -maxLinear, maxLinear)
    }

    cameraHeight = math.pow(1.1, Camera.interpolatedZoom)

    var move = Vector2.Zero

    if (keyInputEnabled) {
      if (AppWindow.keyDown(CameraBind.Up))    move += Vector2(0.0, -1.0)
      if (AppWindow.keyDown(CameraBind.Down))  move += Vector2(0.0, +1.0)
      if (AppWindow.keyDown(CameraBind.Left))  move += Vector2(-1.0, 0.0)
      if (AppWindow.keyDown(CameraBind.Right)) move += Vector2(+1.0, 0.0)
    }

    val relHeight = (Camera.interpolatedZoom - CameraTweak.minZoom) / (CameraTweak.maxZoom - CameraTweak.minZoom)

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

      val moveSpeed = lerp(CameraTweak.moveSpeedLow, CameraTweak.moveSpeedHigh, relHeight)

      cameraVel *= math.pow(0.5, dt / (1.0 / 60.0))
      cameraVel += move * moveSpeed * boost * dt
    } else {
      cameraVel *= math.pow(0.8, dt / (1.0 / 60.0))
    }

    val stopSpeed = lerp(CameraTweak.stopSpeedLow, CameraTweak.stopSpeedHigh, relHeight)

    val velReduce = dt * stopSpeed
    val velLength = cameraVel.length
    if (velLength > velReduce) {
      cameraVel -= (cameraVel / velLength) * velReduce
    } else {
      cameraVel = Vector2.Zero
    }

    Camera.position += cameraVel * dt
  }

  // -- Scene render

  object Camera extends PropertyContainer {
    private val arr = MacroPropertySet.make[Camera.type]()
    override val propertySet: PropertySet = new PropertySet("Camera", arr)

    var position: Vector2Prop.Type = Vector2.Zero
    var zoom: DoubleProp.Type = 40.0
    var interpolatedZoom: DoubleProp.Type = 40.0
  }

  object CameraTweak extends PropertyContainer {
    private val arr = MacroPropertySet.make[CameraTweak.type]()
    override val propertySet: PropertySet = new PropertySet("CameraTweak", arr) {
      range("fov", 30.0, 100.0)
      range("pitchLow", 0.0, 2.0)
      range("pitchHigh", 0.0, 2.0)
    }

    var fov: DoubleProp.Type = 45.0
    var moveSpeedLow: DoubleProp.Type = 600.0
    var moveSpeedHigh: DoubleProp.Type = 1000.0
    var stopSpeedLow: DoubleProp.Type = 6.0
    var stopSpeedHigh: DoubleProp.Type = 10.0
    var moveBoostAmount: DoubleProp.Type = 2.0

    var zoomSpeed: DoubleProp.Type = 3.0
    var minZoom: DoubleProp.Type = 35.0
    var maxZoom: DoubleProp.Type = 45.0

    var zoomInterpolateLinear: DoubleProp.Type = 5.0
    var zoomInterpolateExponential: DoubleProp.Type = 0.2

    var pitchLow: DoubleProp.Type = 0.5
    var pitchHigh: DoubleProp.Type = 0.2
  }

  object DebugView extends PropertyContainer {
    private val arr = MacroPropertySet.make[DebugView.type]()
    override val propertySet: PropertySet = new PropertySet("DebugView", arr)

    var cables: BoolProp.Type = false
    var groundBlockers: BoolProp.Type = false
    var cableParts: BoolProp.Type = false
    var cullRender: BoolProp.Type = false
    var cullGameplay: BoolProp.Type = false
  }

  var cameraPos = Vector3.Zero
  var cameraVel = Vector2.Zero

  var cameraHeight = 0.0
  var view = Matrix43.Identity
  var projection = Matrix4.Identity
  var viewProjection = Matrix4.Identity
  var invViewProjection = Matrix4.Identity

  val visibleEntities = new EntitySet()

  object DepEntities
  object DepProbes
  object DepMeshes
  object DepCables
  object DepGround

  def renderScene(dt: Double): Unit = {

    val frustum = Frustum.fromViewProjection(viewProjection)

    var visibleProbes: ArrayBuffer[Probe] = null
    var visibleMeshes: ModelSystem.MeshInstanceCollection = null
    var visibleCables: ArrayBuffer[CableRenderSystem.CableMeshPart] = null
    var visibleGround: ArrayBuffer[GroundSystem.GroundPlate] = null
    var forwardDraws: ForwardRenderingSystem.Draws = null

    visibleEntities.clear()

    val s = new Scheduler()

    s.add("Dynamic cullables")(cullingSystem)() {
      cullingSystem.updateDynamicCullables()
    }

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

    s.add("Visible towers")(towerSystem, modelSystem)(DepEntities) {
      towerSystem.updateVisible(visibleEntities)
    }

    s.add("Visible animations")(modelSystem, animationSystem)(DepEntities) {
      animationSystem.updateVisibleAnimations(dt, visibleEntities)
    }

    s.addTo("Wire GUI")(Task.Main)(towerSystem, buildSystem)(DepEntities) {
      buildSystem.renderWireGui(canvas, inputs, visibleEntities, viewProjection)
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

    if (DebugView.cables) {
      cableSystem.debugDrawControlPoints(visibleEntities)
    }
    if (DebugView.groundBlockers) {
      cableSystem.debugDrawGroundBlockers()
    }
    if (DebugView.cableParts) {
      val color = Color.rgb(0xFF00FF)
      for (part <- visibleCables) {
        DebugDraw.drawAabb(part.aabb, color)
      }
    }
    if (DebugView.cullRender) {
      cullingSystem.debugDrawCulling(frustum, CullingSystem.MaskRender)
    }
    if (DebugView.cullGameplay) {
      cullingSystem.debugDrawCulling(frustum, CullingSystem.MaskAnyGameplay)
    }

    val renderer = Renderer.get

    renderer.setWriteSrgb(false)
    renderer.setMode(DepthWrite, BlendNone, CullNormal)
    renderer.setRenderTarget(globalRenderSystem.mainTargetMsaa)
    renderer.clear(Some(Color.Black), Some(1.0))

    renderer.pushUniform(GlobalSceneUniform, u => {
      GlobalSceneUniform.ViewProjection.set(u, viewProjection)
    })

    CableShader.get.use()

    for (cable <- visibleCables) {
      cable.draw()
    }

    InstancedMeshShader.get.use()

    for (draw <- forwardDraws.instanced) {
      val mesh = draw.mesh

      renderer.setTexture(InstancedMeshShader.Textures.Albedo, Material.shared.get.missingAlbedo.texture)

      renderer.bindUniform(ModelInstanceUniform, draw.instanceUbo)
      renderer.bindUniform(LightProbeUniform, draw.lightProbeUbo)
      renderer.pushUniform(InstancedMeshShader.VertexUniform, u => {
        import InstancedMeshShader.VertexUniform._
        UvBounds.set(u, mesh.uvOffsetX, mesh.uvOffsetY, mesh.uvScaleX, mesh.uvScaleY)
      })

      renderer.drawElementsInstanced(draw.num, mesh.numIndices, mesh.indexBuffer, mesh.vertexBuffer)
    }

    SkinnedMeshShader.get.use()

    for (draw <- forwardDraws.skinned) {
      val mesh = draw.mesh

      renderer.setTexture(InstancedMeshShader.Textures.Albedo, Material.shared.get.missingAlbedo.texture)

      renderer.bindUniform(SkinnedModelUniform, draw.uniform)
      renderer.pushUniform(SkinnedMeshShader.VertexUniform, u => {
        import SkinnedMeshShader.VertexUniform._
        UvBounds.set(u, mesh.uvOffsetX, mesh.uvOffsetY, mesh.uvScaleX, mesh.uvScaleY)
      })

      renderer.drawElements(mesh.numIndices, mesh.indexBuffer, mesh.vertexBuffer)
    }

    renderer.setTexture(GroundShader.Textures.Albedo, GroundTexture.get.texture)

    GroundShader.get.use()

    for (ground <- visibleGround) {
      ground.draw()
    }

    renderer.setMode(DepthTest, BlendAddAlpha, CullNormal)

    CablePulseShader.get.use()

    renderer.setTexture(CablePulseShader.Textures.Mask, CablePulseMask.get.texture)
    renderer.setTexture(CablePulseShader.Textures.Texture, CablePulseTex.get.texture)

    for (cable <- visibleCables) {
      cable.drawPulse()
    }

    buildSystem.renderIngameGui(viewProjection)

    buildSystem.renderPreview()
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

    hotbarMenu.update(dt)
    towerSystem.update(dt)
    connectionSystem.update(dt)

    if (CableSystem.CableTweak.regenerateCables) {
      cableSystem.regenerateAllCables()
      CableSystem.CableTweak.regenerateCables = false
    }

    buildSystem.setWireGuiEnabled(hotbarMenu.openCategory.exists(_.wireCategory))
    buildSystem.setEntityTypeToBuild(hotbarMenu.selectedItem.map(_.entityType))

    val screenWidth = globalRenderSystem.screenWidth
    val screenHeight = globalRenderSystem.screenHeight
    val aspectRatio = screenWidth.toDouble / screenHeight.toDouble

    val cameraPos = Vector3(Camera.position.x, cameraHeight, Camera.position.y)
    val fov = math.toRadians(CameraTweak.fov)

    val relHeight = (Camera.interpolatedZoom - CameraTweak.minZoom) / (CameraTweak.maxZoom - CameraTweak.minZoom)
    val pitch = lerp(CameraTweak.pitchLow, CameraTweak.pitchHigh, relHeight)

    view = Matrix43.look(cameraPos, Vector3(0.0, -1.0, -pitch))
    projection = Matrix4.perspective(aspectRatio, fov, 1.0, 200.0)
    viewProjection = projection * view
    invViewProjection = viewProjection.inverse

    buildSystem.update(dt, invViewProjection, inputs)
    buildSystem.renderBuildGui(canvas, viewProjection)

    enemySystem.update(dt)

    cableSystem.generateCables()
    entitySystem.processDeletions()

    val renderer = Renderer.get

    renderer.beginFrame()

    if (globalRenderSystem.renderingEnabled) {
      renderScene(dt)

      renderer.setWriteSrgb(false)
      renderer.setMode(DepthWrite, BlendNone, CullNone)
      DebugDraw.render(viewProjection)
    }

    renderer.setWriteSrgb(false)
    renderer.setMode(DepthNone, BlendNone, CullNone)
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

    towerSystem.renderIngameGui(viewProjection)
    canvas.render()

    LayoutDebugger.render()
    DebugDraw.render2D()

    renderer.endFrame()

    AppWindow.swapBuffers()
  }

  override def done: Boolean = finished

}

