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
import com.sun.xml.internal.fastinfoset.DecoderStateTables
import game.component._
import game.menu.HotbarMenu
import game.options.Options
import game.system.{Entity, EntitySet, EntityType}
import game.system.audio.AudioSystem.SoundRef
import game.system.rendering.AmbientSystem.Probe
import game.system.rendering.ModelSystem.{ModelInstance, NodeInstance}
import gfx.{AnimationState, Material}
import io.property._
import io.serialization.{BinaryReader, BinaryWriter}
import menu.gui.TextBox
import task.{Scheduler, Task}
import util.geometry.Frustum
import util.BufferUtils._
import render.Renderer._
import util.BufferIntegrityException

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object PlayState {

  val Colorgrade = TextureAsset("colorgrade/ingame.png.s2tx")
  val ColorgradeGameOver = TextureAsset("colorgrade/game_over.png.s2tx")

  val MusicIdleIntro = SoundAsset("audio/music/ingame_idle_intro.ogg.s2au")
  val MusicIdleLoop = SoundAsset("audio/music/ingame_idle_loop.ogg.s2au")
  val MusicBattleLoop = SoundAsset("audio/music/ingame_battle_loop.ogg.s2au")

  val TestPlaceModel = ModelAsset("game/tower/turret_basic/tower_turret.fbx.s2md")
  val TestPlaceMask = TextureAsset("game/tower/turret_basic/placemask.png.s2tx")
  val NoiseTex = TextureAsset("effect/noise.png.s2tx")

  val CablePulseMask = TextureAsset("effect/pulse/mask/basic.png.s2tx")
  val CablePulseTex = TextureAsset("effect/pulse/tex/hexagon.png.s2tx")

  val CrystalEntity = EntityTypeAsset("entity/tower/crystal.es.toml")

  val GroundAlbedo = TextureAsset("game/ground/ground_albedo.png.s2tx")
  val GroundNormal = TextureAsset("game/ground/ground_normal.png.s2tx")
  val GroundRoughness = TextureAsset("game/ground/ground_roughness.png.s2tx")
  val GroundMetallic = TextureAsset("game/ground/ground_metallic.png.s2tx")
  val GroundAo = TextureAsset("game/ground/ground_ao.png.s2tx")

  val Assets = new AssetBundle("PlayState",
    PauseMenu.Assets,
    TutorialSystem.Assets,
    HotbarMenu.Assets,
    BuildSystem.Assets,
    BulletSystem.Assets,

    CrystalEntity,

    GroundAlbedo,
    GroundNormal,
    GroundMetallic,
    GroundRoughness,
    GroundAo,

    Colorgrade,
    ColorgradeGameOver,

    MusicIdleIntro,
    MusicIdleLoop,
    MusicBattleLoop,

    GroundShader,
    InstancedMeshShader,
    SkinnedMeshShader,
    CableShader,
    CablePulseShader,
    InstancedShadowShader,
    SkinnedShadowShader,

    CablePulseMask,
    CablePulseTex,
  )

  val GameOverDuration = 2.5

  sealed abstract class StartPoint {
    def campaign: EntityTypeAsset
  }

  final case class StartLastSave(preloadInfo: PreloadInfo) extends StartPoint {
    override def campaign: EntityTypeAsset = preloadInfo.campaign
  }
  final case class StartCampaign(campaignAsset: EntityTypeAsset) extends StartPoint {
    override def campaign: EntityTypeAsset = campaignAsset
  }

  case class PreloadInfo(campaign: EntityTypeAsset)

  def preloadSave(availableCampaigns: Iterable[EntityType]): Option[PreloadInfo] = {
    val file = new java.io.File("save.s2sv")
    if (file.exists && file.canRead) {
      val buffer = Memory.alloc(1024 * 16)
      buffer.readFromFile("save.s2sv")
      buffer.finish()

      val campaign: Identifier = try {
        val MinVersion = 3
        val MaxVersion = 3
        buffer.verifyMagic("s2sv")
        buffer.getVersion(MaxVersion, MinVersion)
        buffer.getIdentifier()
      } catch {
        case ex: BufferIntegrityException => Identifier.Empty
      }

      Memory.free(buffer)

      if (campaign != Identifier.Empty) {
        return availableCampaigns
          .flatMap(_.asset)
          .find(_.name == campaign)
          .map(PreloadInfo)
      }
    }

    None
  }

}

class PlayState(val startPoint: StartPoint) extends GameState {

  val inputs = new InputSet()
  val canvas = new Canvas()
  val pauseMenu = new PauseMenu(inputs, canvas)
  val hotbarMenu = new HotbarMenu(inputs, canvas)
  var prevTime = 0.0
  var finished: Boolean = false

  var spawnTime: Double = 0.0

  var musicIdleIntro: SoundRef = null
  var musicIdleLoop: SoundRef = null
  var musicBattleLoop: SoundRef = null

  var crystalEntity: Entity = null
  var gameOverTimer: Double = 0.0

  val campaign: EntityTypeAsset = startPoint.campaign
  var campaignInstance: EntityType = null
  var campaignComponent: CampaignComponent = null

  val BattleFadeDuration = 2.0
  var battleAudioFade: Double = 0.0

  override def load(): Unit = {

    campaign.acquire()
    Assets.acquire()

    GameState.push(new LoadingState())
  }

  override def start(): Unit = {
    campaignInstance = campaign.get
    campaignComponent = campaignInstance.find(CampaignComponent).get

    system.base.loadState()
    system.rendering.loadState()
    system.rendering.loadGame()
    system.gameplay.loadGame()

    pathfindSystem.storeDynamicSnapshot()

    if (campaignComponent.enableTutorial)
      tutorialSystem.startTutorial()

    for (itemC <- campaignInstance.components.collect { case c: ItemComponent => c }) {
      hotbarMenu.addItem(itemC)
    }

    prevTime = AppWindow.currentTime

    val loopLength = MusicIdleLoop.get.lengthInFrames
    val introLength = MusicIdleIntro.get.lengthInFrames

    musicIdleIntro = audioSystem.play(MusicIdleIntro, AudioSystem.Music)
    musicIdleLoop = audioSystem.play(MusicIdleLoop, AudioSystem.Music, startFrame = -introLength)
    musicBattleLoop = audioSystem.play(MusicBattleLoop, AudioSystem.Music, startFrame = loopLength - introLength)
    musicIdleLoop.instance.setFullLoop()
    musicBattleLoop.instance.setFullLoop()
    musicBattleLoop.instance.volume = 0.0

    enemySpawnSystem.setRounds(campaignComponent.spawns)

    startPoint match {
      case StartLastSave(info) =>
        loadGame()
      case _ =>
        enemySpawnSystem.setGameSeed(System.nanoTime.toInt)
    }

    directionalLightSystem.setLight(Vector3(0.25, 0.75, -0.25).normalize, Vector3.One * 1.0)

    if (!campaignComponent.enableTutorial) {
      crystalEntity = entitySystem.create(CrystalEntity.get, Vector3(0.0, 0.0, 0.0))
      ambientPointLightSystem.addLight(crystalEntity, Vector3(0.0, 10.0, 0.0), Vector3(0.5, 0.7, 0.5) * 2.0, 60.0)
    }
  }

  override def stop(): Unit = {
    musicIdleIntro.stop()
    musicIdleLoop.stop()
    musicBattleLoop.stop()

    audioSystem.update()

    if (enemySpawnSystem.allRoundsComplete)
      deleteSaveGame()
      else
      saveGame()

    entitySystem.deleteAllEntities()

    system.gameplay.unloadGame()
    system.rendering.unloadGame()
    system.rendering.unloadState()
    system.base.unloadState()

    Assets.release()
    campaign.release()
  }

  // -- Persistency

  def deleteSaveGame(): Unit = {
    try {
      val file = new java.io.File("save.s2sv")
      file.delete()
    } catch {
      case ex: Exception =>
    }
  }

  def saveGame(): Unit = {
    val buffer = Memory.alloc(1024 * 128)
    val header = Memory.alloc(1024)

    val Version = 3
    header.putMagic("s2sv")
    header.putVersion(Version)
    header.putIdentifier(campaign.name.toString)

    val writer = new BinaryWriter(buffer)

    writer.write(Camera)
    writer.write(buildSystem.persistentState)
    writer.write(tutorialSystem.persistentState)
    writer.write(enemySpawnSystem.persistentState)

    writer.writeHeader(header)

    saveStateSystem.save(buffer)
    pathfindSystem.saveSnapshot(buffer)

    buffer.putMagic("E.sv")

    header.finish()
    buffer.finish()

    val os = new FileOutputStream("save.s2sv")
    header.writeTo(os)
    buffer.writeTo(os)
    os.close()

    Memory.free(buffer)
    Memory.free(header)
  }

  def loadGame(): Unit = {
    val file = new java.io.File("save.s2sv")
    if (file.exists && file.canRead) {
      val buffer = Memory.alloc(1024 * 16)
      buffer.readFromFile("save.s2sv")
      buffer.finish()

      val MinVersion = 3
      val MaxVersion = 3
      buffer.verifyMagic("s2sv")
      val version = buffer.getVersion(MaxVersion, MinVersion)
      buffer.getIdentifier()

      val reader = new BinaryReader(buffer)

      reader.read(Camera)
      reader.read(buildSystem.persistentState)
      reader.read(tutorialSystem.persistentState)
      reader.read(enemySpawnSystem.persistentState)

      saveStateSystem.load(buffer)
      pathfindSystem.loadSnapshot(buffer)

      buffer.verifyMagic("E.sv")

      Memory.free(buffer)

      saveStateSystem.recreateMissingEntities()
      pathfindSystem.applyDynamicSnapshot()
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

      if (!pauseMenu.gameOver)
        pauseMenu.isOpen = !pauseMenu.isOpen
    }

    for (e <- AppWindow.keyDownEvents) {
      if (e.key == KeyEvent.Escape) {
        processEscape()
      }
    }

    if (pauseMenu.ContinueButton.input.clicked && !pauseMenu.gameOver) {
      pauseMenu.isOpen = false
    }

    if (pauseMenu.RetryButton.input.clicked && pauseMenu.gameOver) {
      finished = true
      PlayState.preloadSave(Some(campaign.getShallowUnsafe)) match {
        case Some(info) =>
          val start = StartLastSave(info)
          GameState.push(new PlayState(start))
        case None =>
          GameState.push(new MenuState())
      }
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
    case "build" => buildSystem.persistentState
    case "tutorial" => tutorialSystem.persistentState
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
      Camera.interpolatedZoom += powDt(CameraTweak.zoomInterpolateExponential, dt) * (Camera.zoom - Camera.interpolatedZoom)
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

      cameraVel *= powDt(0.5, dt)
      cameraVel += move * moveSpeed * boost * dt
    } else {
      cameraVel *= powDt(0.8, dt)
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

    Camera.position = Vector2.clamp(Camera.position, CameraTweak.boundsMin, CameraTweak.boundsMax)
    for ((min, max) <- tutorialSystem.cameraArea) {
      Camera.position = Vector2.clamp(Camera.position, min, max)
    }
  }

  // -- Scene render

  object Camera extends PropertyContainer {
    private val arr = MacroPropertySet.make[Camera.type]()
    override val propertySet: PropertySet = new PropertySet("Camera", arr)

    var position: Vector2Prop.Type = Vector2(0.0, 15.0)
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

    var boundsMin: Vector2Prop.Type = Vector2(-150.0, -150.0)
    var boundsMax: Vector2Prop.Type = Vector2(+150.0, +150.0)
  }

  object DebugView extends PropertyContainer {
    private val arr = MacroPropertySet.make[DebugView.type]()
    override val propertySet: PropertySet = new PropertySet("DebugView", arr)

    var cables: BoolProp.Type = false
    var groundBlockers: BoolProp.Type = false
    var cableParts: BoolProp.Type = false
    var cullRender: BoolProp.Type = false
    var cullGameplay: BoolProp.Type = false
    var pathfindFine: BoolProp.Type = false
    var pathfindCoarse: BoolProp.Type = false
  }

  var cameraPos = Vector3.Zero
  var cameraVel = Vector2.Zero

  var cameraHeight = 0.0
  var view = Matrix43.Identity
  var projection = Matrix4.Identity
  var viewProjection = Matrix4.Identity
  var invViewProjection = Matrix4.Identity

  val activeEntitySet = new mutable.HashSet[Entity]()
  val visibleEntities = new EntitySet()
  val shadowEntities = new EntitySet()
  val activeEntities = new EntitySet()

  object DepVisEntities
  object DepShadowEntities
  object DepActiveEntities
  object DepProbes
  object DepVisMeshes
  object DepShadowMeshes
  object DepCables
  object DepGround

  def renderScene(dt: Double): Unit = {

    ambientSystem.globalProbe.clear()
    ambientSystem.globalProbe.addGlobal(Vector3(0.08, 0.08, 0.1) * 1.0)
    ambientSystem.globalProbe.addDirectional(Vector3.Up, Vector3(0.08, 0.08, 0.1) * 1.0)

    val shadowViewProjection = directionalLightSystem.shadowViewProjection
    val frustum = Frustum.fromViewProjection(viewProjection)
    val shadowFrustum = Frustum.fromViewProjection(shadowViewProjection)

    var visibleProbes: ArrayBuffer[Probe] = null
    var visibleTotalProbes: ArrayBuffer[Probe] = null
    var visibleMeshes: ModelSystem.MeshInstanceCollection = null
    var shadowMeshes: ModelSystem.MeshInstanceCollection = null
    var visibleCables: ArrayBuffer[CableRenderSystem.CableMeshPart] = null
    var visibleGround: ArrayBuffer[GroundSystem.GroundPlate] = null
    var forwardDraws: ForwardRenderingSystem.Draws = null
    var shadowDraws: ShadowRenderingSystem.Draws = null

    activeEntitySet.clear()
    visibleEntities.clear()
    shadowEntities.clear()
    activeEntities.clear()

    val s = new Scheduler()

    s.add("Dynamic cullables")(cullingSystem)() {
      cullingSystem.updateDynamicCullables()
    }

    s.add("Viewport cull")(cullingSystem, DepVisEntities)() {
      cullingSystem.cullEntities(visibleEntities, frustum, CullingSystem.MaskRender)
    }

    s.add("Shadow cull")(cullingSystem, DepShadowEntities)() {
      cullingSystem.cullEntities(shadowEntities, shadowFrustum, CullingSystem.MaskShadow)
    }

    s.add("Active merge")(DepActiveEntities)(DepVisEntities, DepShadowEntities) {
      for (e <- visibleEntities.all) {
        if (activeEntitySet.add(e))
          activeEntities.add(e)
      }

      for (e <- shadowEntities.all) {
        if (activeEntitySet.add(e))
          activeEntities.add(e)
      }
    }

    s.add("Probe collect")(ambientSystem, DepProbes)(DepVisEntities) {
      visibleProbes = ambientSystem.updateVisibleProbes(visibleEntities)
    }

    s.add("Ambient point dynamic")(ambientPointLightSystem)() {
      ambientPointLightSystem.updateDynamicLights()
    }

    s.add("Ambient probe points")(DepProbes)(ambientPointLightSystem) {
      ambientPointLightSystem.updateVisibleProbes(visibleProbes)
    }

    s.add("Probe total collect")(ambientSystem, DepProbes)() {
      visibleTotalProbes = ambientSystem.updateTotalProbes(visibleProbes)
    }

    s.add("Directional total probes")(ambientSystem, DepProbes)() {
      directionalLightSystem.updateVisibleProbes(visibleTotalProbes)
    }

    s.add("Ambient probe indirect")(ambientSystem, DepProbes)() {
      ambientSystem.updateIndirectLight(visibleProbes)
    }

    s.add("Visible towers")(towerSystem, modelSystem)(DepActiveEntities) {
      towerSystem.updateVisible(activeEntities)
    }

    s.add("All debris")(debrisSystem)() {
      if (!isPaused)
        debrisSystem.update(dt)
    }

    s.add("Visible debris")(debrisSystem)(DepActiveEntities) {
      debrisSystem.updateVisible(dt, activeEntities)
    }

    s.add("Visible animations")(modelSystem, animationSystem)(DepActiveEntities) {
      animationSystem.updateVisibleAnimations(dt, activeEntities, isPaused)
    }

    s.addTo("Wire GUI")(Task.Main)(towerSystem, buildSystem)(DepActiveEntities) {
      if (!isPaused)
        buildSystem.renderWireGui(canvas, inputs, activeEntities, viewProjection)
    }

    s.add("Visible models")(modelSystem, DepVisMeshes)(DepVisEntities, debrisSystem) {
      val models = modelSystem.collectVisibleModels(visibleEntities)
      modelSystem.updateModels(models)
      visibleMeshes = modelSystem.collectMeshInstances(models)
    }

    s.add("Shadow models")(modelSystem, DepShadowMeshes)(DepShadowEntities, debrisSystem) {
      val models = modelSystem.collectVisibleModels(shadowEntities)
      modelSystem.updateModels(models)
      shadowMeshes = modelSystem.collectMeshInstances(models)
    }

    s.add("Collect cables")(cableRenderSystem, DepCables)(DepVisEntities) {
      visibleCables = cableRenderSystem.collectCableMeshes(visibleEntities)
    }

    s.add("Collect ground")(groundSystem, DepGround)(DepVisEntities) {
      visibleGround = groundSystem.collectGroundPlates(visibleEntities)
    }

    s.addTo("Forward draws")(Task.Main)(forwardRenderingSystem)(DepVisMeshes, DepProbes) {
      forwardDraws = forwardRenderingSystem.createMeshDraws(visibleMeshes)
    }

    s.addTo("Shadow draws")(Task.Main)(shadowRenderingSystem)(DepShadowMeshes) {
      shadowDraws = shadowRenderingSystem.createMeshDraws(shadowMeshes)
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
    if (DebugView.pathfindCoarse || DebugView.pathfindFine) {
      pathfindSystem.debugDraw(DebugView.pathfindFine, DebugView.pathfindCoarse)
    }

    val renderer = Renderer.get

    renderer.setMode(DepthWrite, BlendNone, CullNormal)
    renderer.setRenderTarget(directionalLightSystem.shadowTarget)
    renderer.clear(None, Some(1.0))

    renderer.pushUniform(GlobalSceneUniform, u => {
      GlobalSceneUniform.ViewProjection.set(u, shadowViewProjection)
      GlobalSceneUniform.ViewPosition.set(u, 0.0f, 0.0f, 0.0f, 0.0f)
    })

    InstancedShadowShader.get.use()

    for (draw <- shadowDraws.instanced) {
      val mesh = draw.mesh

      renderer.bindUniform(ShadowInstanceUniform, draw.instanceUbo)
      renderer.drawElementsInstanced(draw.num, mesh.numIndices, mesh.indexBuffer, mesh.vertexBuffer)
    }

    for (draw <- forwardDraws.skinned) {
      val mesh = draw.mesh

      SkinnedShadowShader.get.use(p => {
        import SkinnedShadowShader.Permutations._
        p(BonesPerVertex) = SkinnedShadowShader.getBonePermutation(mesh.maxBonesPerVertex)
      })

      renderer.bindUniform(SkinnedShadowUniform, draw.uniform)
      renderer.drawElements(mesh.numIndices, mesh.indexBuffer, mesh.vertexBuffer)
    }

    renderer.setWriteSrgb(false)
    renderer.setMode(DepthWrite, BlendNone, CullNormal)
    renderer.setRenderTarget(globalRenderSystem.mainTargetMsaa)
    renderer.clear(Some(Color.Black), Some(1.0))

    renderer.pushUniform(GlobalSceneUniform, u => {
      val sw = directionalLightSystem.shadowTarget.width.toDouble
      val sh = directionalLightSystem.shadowTarget.height.toDouble
      val isw = 1.0 / sw
      val ish = 1.0 / sh

      GlobalSceneUniform.ViewProjection.set(u, viewProjection)
      GlobalSceneUniform.ShadowViewProjection.set(u, shadowViewProjection)
      GlobalSceneUniform.ViewPosition.set(u, cameraPos, 0.0f)
      GlobalSceneUniform.ShadowSize.set(u, sw.toFloat, sh.toFloat, isw.toFloat, ish.toFloat)
      GlobalSceneUniform.LightDirection.set(u, directionalLightSystem.lightDirection, 0.0f)
      GlobalSceneUniform.LightIntensity.set(u, directionalLightSystem.lightIntensity, 0.0f)
    })

    CableShader.get.use()
    renderer.setTextureTargetDepth(CableShader.Textures.ShadowMap, directionalLightSystem.shadowTarget)

    for (cable <- visibleCables) {
      cable.draw()
    }

    InstancedMeshShader.get.use()

    renderer.setTextureTargetDepth(InstancedMeshShader.Textures.ShadowMap, directionalLightSystem.shadowTarget)

    for (draw <- forwardDraws.instanced) {
      val mesh = draw.mesh
      val mat = mesh.mesh.material

      val Tex = InstancedMeshShader.Textures
      renderer.setTexture(Tex.MatAlbedo, mat.albedoTex.texture)
      renderer.setTexture(Tex.MatNormal, mat.normalTex.texture)
      renderer.setTexture(Tex.MatRoughness, mat.roughnessTex.texture)
      renderer.setTexture(Tex.MatMetallic, mat.metallicTex.texture)
      renderer.setTexture(Tex.MatAo, mat.aoTex.texture)

      renderer.bindUniform(ModelInstanceUniform, draw.instanceUbo)
      renderer.bindUniform(LightProbeUniform, draw.lightProbeUbo)
      renderer.pushUniform(InstancedMeshShader.VertexUniform, u => {
        import InstancedMeshShader.VertexUniform._
        UvBounds.set(u, mesh.uvOffsetX, mesh.uvOffsetY, mesh.uvScaleX, mesh.uvScaleY)
      })

      renderer.drawElementsInstanced(draw.num, mesh.numIndices, mesh.indexBuffer, mesh.vertexBuffer)
    }

    for (draw <- forwardDraws.skinned) {
      val mesh = draw.mesh
      val mat = mesh.mesh.material

      SkinnedMeshShader.get.use(p => {
        import SkinnedMeshShader.Permutations._
        p(BonesPerVertex) = SkinnedMeshShader.getBonePermutation(mesh.maxBonesPerVertex)
      })

      val Tex = SkinnedMeshShader.Textures
      renderer.setTexture(Tex.MatAlbedo, mat.albedoTex.texture)
      renderer.setTexture(Tex.MatNormal, mat.normalTex.texture)
      renderer.setTexture(Tex.MatRoughness, mat.roughnessTex.texture)
      renderer.setTexture(Tex.MatMetallic, mat.metallicTex.texture)
      renderer.setTexture(Tex.MatAo, mat.aoTex.texture)
      renderer.setTextureTargetDepth(Tex.ShadowMap, directionalLightSystem.shadowTarget)

      renderer.bindUniform(SkinnedModelUniform, draw.uniform)
      renderer.pushUniform(SkinnedMeshShader.VertexUniform, u => {
        import SkinnedMeshShader.VertexUniform._
        UvBounds.set(u, mesh.uvOffsetX, mesh.uvOffsetY, mesh.uvScaleX, mesh.uvScaleY)
      })

      renderer.drawElements(mesh.numIndices, mesh.indexBuffer, mesh.vertexBuffer)
    }

    renderer.setTexture(GroundShader.Textures.MatAlbedo, GroundAlbedo.get.texture)
    renderer.setTexture(GroundShader.Textures.MatNormal, GroundNormal.get.texture)
    renderer.setTexture(GroundShader.Textures.MatRoughness, GroundRoughness.get.texture)
    renderer.setTexture(GroundShader.Textures.MatMetallic, GroundMetallic.get.texture)
    renderer.setTexture(GroundShader.Textures.MatAo, GroundAo.get.texture)
    renderer.setTextureTargetDepth(GroundShader.Textures.ShadowMap, directionalLightSystem.shadowTarget)

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

    bulletSystem.renderBullets(viewProjection, cameraPos)

    buildSystem.renderIngameGui(viewProjection)

    buildSystem.renderPreview()
  }

  override def update(): Unit = {

    AppWindow.pollEvents()

    val time = AppWindow.currentTime
    var dt = time - prevTime
    prevTime = time

    if (AppWindow.keyDown(KeyEvent.Tab)) {
      if (AppWindow.keyDown(KeyEvent.LeftShift)) {
        dt *= 0.05
      } else {
        dt *= 0.2
      }
    }

    inputs.update()

    if (crystalEntity != null && crystalEntity.hasFlag(Entity.Flag_Deleted)) {
      gameOverTimer += dt
      if (gameOverTimer >= GameOverDuration) {
        pauseMenu.isOpen = true
        pauseMenu.gameOver = true
      }
    }

    if (enemySpawnSystem.allRoundsComplete) {
      pauseMenu.isOpen = true
      pauseMenu.gameOver = true
      pauseMenu.gameWon = true
    }

    if ((pauseSystem.paused && !pauseMenu.gameOver) || pauseMenu.gameWon) {
      battleAudioFade -= dt / BattleFadeDuration
    } else {
      battleAudioFade += dt / BattleFadeDuration
    }
    battleAudioFade = clamp(battleAudioFade, 0.0, 1.0)

    {
      val fade = smoothStep(battleAudioFade)
      musicIdleIntro.instance.volume = 1.0 - fade
      musicIdleLoop.instance.volume = 1.0 - fade
      musicBattleLoop.instance.volume = fade
    }

    pauseSystem.update(dt)

    updatePauseMenu(dt)
    updateDebugMenu()

    if (!isPaused) {
      tutorialSystem.update(dt)
      updateCameraMovement(dt)
    }

    audioSystem.update()

    tutorialSystem.render(canvas, inputs)

    if (!pauseMenu.gameOver) {

      if (pauseSystem.paused)
        hotbarMenu.update(dt)

      pauseSystem.renderGui(canvas, inputs)
    }

    if (!isPaused) {
      enemySystem.update(dt)
      towerSystem.update(dt)
      connectionSystem.update(dt)
    }

    if (CableSystem.CableTweak.regenerateCables) {
      cableSystem.regenerateAllCables()
      CableSystem.CableTweak.regenerateCables = false
    }

    buildSystem.setWireGuiEnabled(hotbarMenu.openCategory.exists(_.wireCategory))
    buildSystem.setEntityTypeToBuild(hotbarMenu.selectedItem.map(_.entityType.get))

    if (hotbarMenu.openCategory.contains(hotbarMenu.turretCategory)) {
      tutorialSystem.progress(TutorialSystem.SelectTurretCategory, 1.0)
    }
    if (hotbarMenu.selectedItem.exists(_.entityType.name.toString.contains("turret_basic"))) {
      tutorialSystem.progress(TutorialSystem.SelectTurret, 1.0)
    }
    if (hotbarMenu.selectedItem.exists(_.entityType.name.toString.contains("radar_tutorial"))) {
      tutorialSystem.progress(TutorialSystem.SelectRadar, 1.0)
    }
    if (hotbarMenu.selectedItem.exists(_.entityType.name.toString.contains("turret_directed"))) {
      tutorialSystem.progress(TutorialSystem.SelectDirectedTurret, 1.0)
    }
    if (hotbarMenu.openCategory.isEmpty && hotbarMenu.selectedItem.isEmpty) {
      tutorialSystem.progress(TutorialSystem.BuildNoSelection, 1.0)
    }

    val screenWidth = globalRenderSystem.screenWidth
    val screenHeight = globalRenderSystem.screenHeight
    val aspectRatio = screenWidth.toDouble / screenHeight.toDouble

    cameraPos = Vector3(Camera.position.x, cameraHeight, Camera.position.y)
    val fov = math.toRadians(CameraTweak.fov)

    val relHeight = (Camera.interpolatedZoom - CameraTweak.minZoom) / (CameraTweak.maxZoom - CameraTweak.minZoom)
    val pitch = lerp(CameraTweak.pitchLow, CameraTweak.pitchHigh, relHeight)
    val cameraDir = Vector3(0.0, -1.0, -pitch)

    view = Matrix43.look(cameraPos, cameraDir)
    projection = Matrix4.perspective(aspectRatio, fov, 1.0, 200.0)
    viewProjection = projection * view
    invViewProjection = viewProjection.inverse

    directionalLightSystem.setupShadowProjection(cameraPos, cameraDir)

    if (!isPaused) {
      buildSystem.update(dt, invViewProjection, inputs)
      buildSystem.renderBuildGui(canvas, viewProjection)
      bulletSystem.updateBullets(dt)
    }

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

    val colorgrade = if (pauseMenu.gameOver && !pauseMenu.gameWon) {
      ColorgradeGameOver
    } else {
      Colorgrade
    }
    renderer.setTexture(TonemapShader.Textures.ColorLookup, colorgrade.get.texture)

    renderer.drawQuad()

    renderer.setRenderTarget(RenderTarget.Backbuffer)
    renderer.clear(Some(Color.Black), None)

    renderer.setWriteSrgb(false)
    PostprocessShader.get.use()

    renderer.setTextureTargetColor(TonemapShader.Textures.Backbuffer, globalRenderSystem.msaaResolveTarget, 0)

    renderer.drawQuad()

    renderer.setWriteSrgb(true)

    if (!isPaused)
      towerSystem.renderIngameGui(viewProjection)

    canvas.render()

    LayoutDebugger.render()
    DebugDraw.render2D()

    renderer.endFrame()

    AppWindow.swapBuffers()
  }

  override def done: Boolean = finished

}

