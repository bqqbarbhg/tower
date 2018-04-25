package game.system.gameplay

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import core._
import render._
import asset._
import game.component._
import game.system._
import game.system.Entity._
import game.system.base._
import game.system.audio._
import game.system.rendering._
import game.system.rendering.ModelSystem._
import game.shader.PlaceMeshShader
import BuildSystemImpl._
import core.Matrix4
import game.options.Options
import game.system.gameplay.TowerSystem.Slot
import game.system.rendering.CullingSystem.RayHit
import io.property._
import locale.Locale
import locale.LocaleString._
import platform.{AppWindow, KeyEvent}
import render.Renderer._
import ui.Canvas.TextStyle
import ui.InputSet.InputArea
import ui.LineBatch.HermiteNode
import ui.SpriteBatch.SpriteDraw
import ui._
import util.geometry._

object BuildSystem {

  val Assets = new AssetBundle("BuildSystem",
    PlaceMeshShader,
    NoiseTex,
    HudAtlas,
    WireAtlas,
    MenuAtlas,
    IngameAtlas,
    FailPlaceSound,
    SlotFont,
    BreakSound,
    LineBatch.LineShader,
  )

}

sealed trait BuildSystem extends EntityDeleteListener {

  /** Set the entity to present to the user as buildable.
    * May be called multiple times with the same entity. */
  def setEntityTypeToBuild(entityType: Option[EntityType])

  /** Mark area in space as used */
  def addGridBlocker(entity: Entity, component: GridBlockComponent): Unit

  /** Tick the system */
  def update(dt: Double, invViewProj: Matrix4, inputs: InputSet): Unit

  /** Render build preview */
  def renderPreview(): Unit

  /** Enable/disable wire GUI */
  def setWireGuiEnabled(enabled: Boolean): Unit

  /** Render GUI used for wiring towers */
  def renderWireGui(canvas: Canvas, inputs: InputSet, visible: EntitySet, viewProjection: Matrix4): Unit

  /** Render GUI elements used by this system */
  def renderBuildGui(canvas: Canvas, viewProjection: Matrix4): Unit

  /** Render the in-game overlay GUI elements */
  def renderIngameGui(viewProjection: Matrix4): Unit

  /** Add currency that the player can use to build more stuff */
  def addMoney(amount: Int): Unit

  /** Release resources used by the system */
  def unload(): Unit

  /** Retrieve persistent state for save/load */
  def persistentState: PropertyContainer
}

object BuildSystemImpl {

  val NoiseTex = TextureAsset("effect/noise.png.s2tx")

  class BuildPreview(val entityType: EntityType, val entity: Entity, val comp: BuildPreviewComponent) {
    val model = ModelAsset(comp.model)
    val mask = TextureAsset(comp.mask)
    var time: Double = 0.0
    var modelInst: ModelInstance = _
    var valid: Boolean = false
    var visible: Boolean = false

    def delete(): Unit = {
      entity.delete()
    }
  }

  val GroundPlane = Plane(Vector3.Up, 0.0)

  val MaxBlockers = 16
  val MaxRayCast = 16

  val MenuAtlas = AtlasAsset("atlas/menu.s2at")
  val HudAtlas = AtlasAsset("atlas/hud.s2at")
  val WireAtlas = AtlasAsset("atlas/wire.s2at")
  val IngameAtlas = AtlasAsset("atlas/ingame.s2at")
  val FailPlaceSound = SoundAsset("audio/effect/error.wav.s2au")

  val SlotFont = FontAsset("font/catamaran/Catamaran-SemiBold.ttf.s2ft")
  val SlotEmptySprite = Identifier("gui/menu/slot_empty.png")
  val SlotFullSprite = Identifier("gui/menu/slot_full.png")

  val MenuTitleFont = FontAsset("font/catamaran/Catamaran-SemiBold.ttf.s2ft")
  val MenuTextFont = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")

  val SlotHudEmptySprite = Identifier("gui/hud/cable_slot_empty.png")
  val SlotHudFullSprite = Identifier("gui/hud/cable_slot_full.png")
  val SlotHudIncompatibleEmptySprite = Identifier("gui/hud/cable_slot_incompatible_empty.png")
  val SlotHudIncompatibleFullSprite = Identifier("gui/hud/cable_slot_incompatible_full.png")

  val WireSprite = Identifier("gui/wire/plain.png")

  val TowerSelectSprite = Identifier("gui/ingame/tower_select.png")
  val IngameBlockerSprite = Identifier("gui/ingame/blocker.png")

  val PreviewValidColor = Color.rgba(0xFFFFFF, 1.0)
  val PreviewBadColor = Color.rgba(0xFF0000, 1.0)

  val BreakSound = SoundAsset("audio/effect/break.wav.s2au")

  val BlockerSprite = Identifier("gui/hud/build_blocker.png")
  val CenterAnchor = Vector2(0.5, 0.5)
  val NoSlots = Array[Slot]()
  val NoCenters = Array[Vector2]()

  val WireBackColor = Color.rgba(0x000000, 0.5)
  val SlotIdleColor = Color.rgba(0xFFFFFF, 0.5)
  val SlotHoverColor = Color.rgba(0xFFFFFF, 1.0)

  val WireBaseText = TextStyle(SlotFont, 24.0)
  val WireInputText = WireBaseText.copy(align = Canvas.Align(0.0, 0.5))
  val WireOutputText = WireBaseText.copy(align = Canvas.Align(1.0, 0.5))

  val CurrentMoneyText = TextStyle(MenuTitleFont, 24.0)
  val ObjectNameText = TextStyle(MenuTitleFont, 24.0)
  val ObjectPriceText = TextStyle(MenuTitleFont, 20.0)
  val ObjectDescText = TextStyle(MenuTitleFont, 14.0)

  class WireGui(val entity: Entity) {
    var slots: Seq[Slot] = NoSlots
    var slotCenters: Array[Vector2] = NoCenters
    val iconInput = new InputArea()
  }

  case class SlotRef(gui: WireGui, slot: Int)
  case class HudSlotRef(position: Vector3, slot: Slot)

  val BuildRotations = Array(
    Quaternion.fromAxisAngle(Vector3.Up, math.Pi * -0.0),
    Quaternion.fromAxisAngle(Vector3.Up, math.Pi * -0.5),
    Quaternion.fromAxisAngle(Vector3.Up, math.Pi * -1.0),
    Quaternion.fromAxisAngle(Vector3.Up, math.Pi * -1.5),
  )

  val BuildRotationSwapXY = Array(false, true, false, true)
  val GridSize = 4.0

  object PersistentState {
    private val arr = MacroPropertySet.make[PersistentState]()
    private val propertySet: PropertySet = new PropertySet("BuildSystem.PersistentState", arr)
  }

  class PersistentState extends PropertyContainer {
    override def propertySet: PropertySet = PersistentState.propertySet

    /** Current money balance */
    var currentMoney: IntProp.Type = 0

  }

  val ForbiddenBuildAreaColor = Color.rgba(0xFF0000, 0.3)

}

final class BuildSystemImpl extends BuildSystem {
  var buildEntity: Option[EntityType] = None
  var buildPreview: Option[BuildPreview] = None
  var buildRotation: Quaternion = Quaternion.Identity
  var buildRotationIndex: Int = 0

  var prevMouseDown: Boolean = false
  var buildBlockerEntities = new mutable.HashSet[Entity]()
  var rayCastResult = new ArrayBuffer[RayHit]()
  var failCooldown: Double = 0.0
  var prevWireGui = mutable.HashMap[Entity, WireGui]()
  var wireGuiEnabled = false
  var selectedTower: Option[Entity] = None
  var hoveredTower: Option[Entity] = None
  var prevHoveredTower: Option[Entity] = None
  var towerHoverTime: Double = 0.0

  var hudSlotEntity: Entity = null
  var hudSlotInput = new InputArea()

  val deleteBind = KeyEvent.NameToKey.get(Options.current.binds.delete).getOrElse(KeyEvent.Delete)
  val rotateBind = KeyEvent.NameToKey.get(Options.current.binds.rotate).getOrElse('R'.toInt)

  val ingameGuiWorld = Matrix43.world(Vector3(1.0, 0.0, 0.0), Vector3(0.0, 0.0, 1.0), Vector3(0.0, 1.0, 0.0), Vector3(0.0, 2.0, 0.0))

  var activeSlot: Option[SlotRef] = None
  var activeHudSlot: Option[Slot] = None

  val gridOccupied = new mutable.HashMap[(Int, Int), Entity]()
  val entityToGridArea = new mutable.HashMap[Entity, (Int, Int, Int, Int)]()

  val lineBatch = new LineBatch()

  val persistent = new PersistentState()

  val QuadSprite = Identifier("gui/menu/background_white.png")

  override def persistentState: PropertyContainer = persistent

  def makePreview(entityType: EntityType): Option[BuildPreview] = {
    for (comp <- entityType.find(BuildPreviewComponent)) yield {
      val entity = new Entity(false, "Build preview")
      val pre = new BuildPreview(entityType, entity, comp)
      pre.modelInst = modelSystem.addModel(pre.entity, pre.model)
      pre
    }
  }

  override def setEntityTypeToBuild(entityType: Option[EntityType]): Unit = {
    if (entityType != buildEntity) {
      buildEntity = entityType
      for (pre <- buildPreview) pre.delete()
      entityType match {
        case Some(et) => buildPreview = makePreview(et)
        case None => buildPreview = None
      }
    }
  }

  def collectGridIntersecting(result: mutable.HashSet[Entity], x: Int, y: Int, w: Int, h: Int): Unit = {
    for (xx <- x until x + w; yy <- y until y + h) {
      gridOccupied.get(xx, yy) match {
        case Some(e) => result += e
        case None =>
      }
    }
  }

  def setGridOccupied(entity: Entity, x: Int, y: Int, w: Int, h: Int): Unit = {
    entity.setFlag(Flag_GridOccupier)
    entityToGridArea(entity) = (x, y, w, h)
    for (xx <- x until x + w; yy <- y until y + h) {
      gridOccupied((xx, yy)) = entity
    }
  }

  def clearGridOccupied(x: Int, y: Int, w: Int, h: Int): Unit = {
    for (xx <- x until x + w; yy <- y until y + h) {
      gridOccupied.remove((xx, yy))
    }
  }

  /** Mark area in space as used */
  override def addGridBlocker(entity: Entity, component: GridBlockComponent): Unit = {
    val point = entity.position

    val swapXY = math.abs(entity.rotation.rotate(Vector3(1.0, 0.0, 0.0)).x) < 0.5

    val (w, h) = if (swapXY) {
      (component.height, component.width)
    } else {
      (component.width, component.height)
    }
    val offX = 0.5 * (w - 1)
    val offY = 0.5 * (h - 1)
    val gridX = math.round(point.x / GridSize - offX).toInt
    val gridZ = math.round(point.z / GridSize - offY).toInt

    setGridOccupied(entity, gridX, gridZ, w, h)
  }

  override def update(dt: Double, invViewProj: Matrix4, inputs: InputSet): Unit = {

    if (!pauseSystem.paused) {
      activeSlot = None
      activeHudSlot = None
      selectedTower = None
      hoveredTower = None
      prevHoveredTower = None
      return
    }

    if (activeHudSlot.isEmpty && activeSlot.isEmpty) {
      tutorialSystem.progress(TutorialSystem.BuildNoCable, 1.0)
    }

    val mouseDown = AppWindow.mouseButtonDown(0)
    val clicked = mouseDown && !prevMouseDown && inputs.focusedLayer < -1000

    if (AppWindow.mouseButtonDown(1)) {
      activeSlot = None
      activeHudSlot = None
    }

    buildBlockerEntities.clear()
    if (failCooldown > 0.0) {
      failCooldown -= dt
    }

    val mousePos = AppWindow.mousePosition
    val relX = mousePos.x / globalRenderSystem.screenWidth
    val relY = mousePos.y / globalRenderSystem.screenHeight
    val ndcX = relX * 2.0 - 1.0
    val ndcY = -(relY * 2.0 - 1.0)

    val near = invViewProj.projectPoint(Vector3(ndcX, ndcY, -1.0))
    val far = invViewProj.projectPoint(Vector3(ndcX, ndcY, +1.0))

    val ray = Ray.fromUnnormalized(near, far - near)

    for (buildE <- buildEntity) {
      val buildC = buildE.find(BuildableComponent).get
      val gridC = buildE.find(GridBlockComponent).get

      selectedTower = None

      if (tutorialSystem.allowRotate) {
        for (e <- AppWindow.keyDownEvents if e.key == rotateBind) {
          buildRotationIndex = (buildRotationIndex + 1) % BuildRotations.length
          buildRotation = BuildRotations(buildRotationIndex)
        }
      }

      val t = ray.intersect(GroundPlane)
      var groundPoint = t.map(ray.point).map(point => {
        val (w, h) = if (BuildRotationSwapXY(buildRotationIndex)) {
          (gridC.height, gridC.width)
        } else {
          (gridC.width, gridC.height)
        }
        val offX = 0.5 * (w - 1)
        val offY = 0.5 * (h - 1)
        val gridX = math.round(point.x / GridSize - offX).toInt
        val gridZ = math.round(point.z / GridSize - offY).toInt
        val roundX = (gridX + offX) * GridSize
        val roundZ = (gridZ + offY) * GridSize

        ((gridX, gridZ, w, h), Vector3(roundX, 0.0, roundZ))
      })

      if (inputs.focusedLayer >= -1000)
        groundPoint = None

      var validPlace = false

      for (((gridX, gridZ, gridW, gridH), point) <- groundPoint) {
        collectGridIntersecting(buildBlockerEntities, gridX, gridZ, gridW, gridH)
        validPlace = buildBlockerEntities.isEmpty

        if (buildC.price > persistent.currentMoney) {
          validPlace = false
        }

        for (allowed <- tutorialSystem.allowedBuilding) {
          if (!buildE.name.contains(allowed)) {
            validPlace = false
          }
        }

        for (allowed <- tutorialSystem.allowedRotationIndex) {
          if (buildRotationIndex != allowed) {
            validPlace = false
          }
        }

        for ((min, max) <- tutorialSystem.buildArea) {
          if (point.x < min.x || point.z < min.y || point.x > max.x || point.z > max.y)
            validPlace = false
        }

        if (clicked) {
          if (validPlace) {
            persistent.currentMoney -= buildC.price

            val entity = entitySystem.create(buildE, point, buildRotation)
            saveStateSystem.registerSavedTower(entity)

            for (buildC <- buildE.find(BuildableComponent)) {
              val sound = SoundAsset(buildC.placeSound)
              audioSystem.play(sound, AudioSystem.Sfx)
            }

            tutorialSystem.progress(TutorialSystem.BuildAny, 1.0)

          } else {
            if (failCooldown <= 0.0) {
              audioSystem.play(FailPlaceSound, AudioSystem.Ui)
              failCooldown = 0.25
            }
          }
        }
      }

      for (preview <- buildPreview) {
        for ((_, point) <- groundPoint) {
          preview.entity.position = point + Vector3(0.0, 0.01, 0.0)
          preview.entity.rotation = buildRotation
        }
        preview.time += dt
        preview.valid = validPlace
        preview.visible = groundPoint.nonEmpty
      }
    }

    if (clicked)
      selectedTower = None

    if (buildEntity.isEmpty && inputs.focusedLayer < -1000) {
      prevHoveredTower = hoveredTower
      hoveredTower = None

      rayCastResult.clear()
      cullingSystem.queryRay(ray, Double.PositiveInfinity, MaxRayCast, CullingSystem.MaskTower, rayCastResult)
      val towers = rayCastResult.iterator.filter(_.entity.hasFlag(Flag_Tower))
      if (towers.nonEmpty) {
        val closest = towers.minBy(_.t)

        if (clicked) {
          selectedTower = Some(closest.entity)
          activeSlot = None
          activeHudSlot = None
        }
        hoveredTower = Some(closest.entity)
      }

      if (prevHoveredTower != hoveredTower) {
        towerHoverTime = 0.0
      } else {
        towerHoverTime += dt
      }
    } else {
      towerHoverTime += dt
    }

    for (selected <- selectedTower) {

      if (AppWindow.keyDownEvents.exists(_.key == deleteBind)) {
        val allowDelete = tutorialSystem.allowedDeleteTarget.forall(selected.name.contains(_))

        if (allowDelete) {
          for (buildC <- selected.prototype.find(BuildableComponent)) {
            persistent.currentMoney += buildC.price
          }

          for (breakComp <- selected.prototype.find(BreakableComponent)) {
            if (breakComp.softBreakEffect.nonEmpty) {
              val prototype = EntityTypeAsset(breakComp.softBreakEffect).get
              entitySystem.createEffect(prototype, selected.position, selected.rotation)
            }
          }

          saveStateSystem.unregisterSavedTower(selected)

          audioSystem.play(BreakSound, AudioSystem.Sfx)
          selected.delete()
          selectedTower = None

          tutorialSystem.progress(TutorialSystem.BuildAnyDelete, 1.0)
        }
      }

    }

    prevMouseDown = mouseDown
  }

  override def renderPreview(): Unit = {
    val renderer = Renderer.get

    renderer.setMode(DepthTest, BlendAddAlpha, CullNormal)

    for (preview <- buildPreview if preview.visible) {
      val models = modelSystem.collectModels(preview.entity)
      modelSystem.updateModels(models)
      val meshes = modelSystem.collectMeshInstances(models)

      PlaceMeshShader.get.use()
      renderer.setTexture(PlaceMeshShader.Textures.Noise, NoiseTex.get.texture)

      val color = if (preview.valid) PreviewValidColor else PreviewBadColor

      for {
        (mesh, insts) <- meshes.meshes
        inst <- insts
      } {
        renderer.setTexture(PlaceMeshShader.Textures.Placemask, preview.mask.get.texture)

        renderer.pushUniform(PlaceMeshShader.PixelUniform, u => {
          import PlaceMeshShader.PixelUniform._
          val time = preview.time.toFloat
          UvOffset.set(u, time, 0.0f, 0.0f, 0.0f)
          Color.set(u, color)
        })

        renderer.pushUniform(PlaceMeshShader.VertexUniform, u => {
          import PlaceMeshShader.VertexUniform._
          World.set(u, inst.worldTransform)
          UvBounds.set(u, mesh.uvOffsetX, mesh.uvOffsetY, mesh.uvScaleX, mesh.uvScaleY)
        })

        mesh.draw()
      }
    }
  }

  override def setWireGuiEnabled(enabled: Boolean): Unit = {
    if (wireGuiEnabled == enabled) return
    wireGuiEnabled = enabled
  }

  def connectSlots(a: Slot, b: Slot): Unit = {
    towerSystem.connectSlots(a, b)
    saveStateSystem.registerConnection(a, b)

    if (a != b)
      tutorialSystem.progress(TutorialSystem.BuildAnyCable, 1.0)
  }

  override def renderWireGui(canvas: Canvas, inputs: InputSet, visible: EntitySet, viewProjection: Matrix4): Unit = {
    if (!pauseSystem.paused) return

    val nextWireGui = new mutable.HashMap[Entity, WireGui]()

    def makeWireGui(entity: Entity): Unit = {
      val slots = towerSystem.getSlots(entity)
      if (slots.nonEmpty) {
        val gui = prevWireGui.getOrElse(entity, new WireGui(entity))
        gui.slots = slots
        gui.slotCenters = Array.fill(slots.length)(Vector2.Zero)
        nextWireGui(entity) = gui
      }
    }

    if (wireGuiEnabled) {
      for (entity <- visible.flag(Flag_Slots)) {
        makeWireGui(entity)
      }
    }

    // Active GUI is always visible
    for (active <- activeSlot) {
      nextWireGui(active.gui.entity) = active.gui
    }

    // Selected tower's GUI is visible
    if (false) {
      for (selected <- selectedTower if selected.hasFlag(Flag_Slots)) {
        if (!nextWireGui.contains(selected)) {
          makeWireGui(selected)
        }
      }
    }

    for ((entity, wireGui) <- nextWireGui) {
      val clickIndex = wireGui.iconInput.clickIndex
      if (clickIndex >= 0) {
        activeSlot match {
          case Some(slot) =>
            connectSlots(slot.gui.slots(slot.slot), wireGui.slots(clickIndex))
            activeSlot = None
            selectedTower = None

          case None =>
            activeSlot = Some(SlotRef(wireGui, clickIndex))
            tutorialSystem.progress(TutorialSystem.StartBuildCable, 1.0)
        }
      }
    }

    for ((entity, wireGui) <- nextWireGui) {
      val projected = viewProjection.projectPoint(entity.position + Vector3(0.0, 3.0, 0.0))
      val x = (projected.x + 1.0) * 0.5 * globalRenderSystem.screenWidth
      val y = (1.0 - (projected.y + 1.0) * 0.5) * globalRenderSystem.screenHeight

      val unit = globalRenderSystem.screenHeight * (1.0 / 720.0)
      val width = unit * (200.0 + 2.0 * 10.0)
      val height = unit * (20.0 * 6.0 + 2.0 * 10.0)

      var minX = clamp(x - width * 0.5, 0.0, globalRenderSystem.screenWidth - width)
      var minY = clamp(y - height * 0.5, 0.0, globalRenderSystem.screenHeight - height)

      val layout = new Layout(Vector2(unit, unit), minX, minY, minX + width, minY + height)

      inputs.addBlocker(-10, layout)

      val mutLayout = layout.copy.padAround(10.0)
      val inputLayout = mutLayout.pushLeft(100.0)
      val outputLayout = mutLayout.pushLeft(100.0)

      canvas.draw(2, QuadSprite, layout, WireBackColor)

      for ((slot, index) <- wireGui.slots.zipWithIndex) {
        val parent = if (slot.isInput) inputLayout else outputLayout
        val area = parent.pushTop(20.0)
        val mutArea = area.copy

        val text = lc"${slot.info.locale}.name"

        val iconArea = if (slot.isInput) {
          val icon = mutArea.pushLeft(20.0)
          mutArea.padLeft(5.0)
          canvas.drawText(3, WireInputText, mutArea, text)
          icon
        } else {
          val icon = mutArea.pushRight(20.0)
          mutArea.padRight(5.0)
          canvas.drawText(3, WireOutputText, mutArea, text)
          icon
        }

        inputs.add(3, wireGui.iconInput, iconArea, 4.0, index)

        wireGui.slotCenters(index) = iconArea.center

        val focused = wireGui.iconInput.focusIndex == index
        val color = if (focused) SlotHoverColor else SlotIdleColor
        val sprite = if (slot.connection.isDefined) SlotFullSprite else SlotEmptySprite
        canvas.draw(3, sprite, iconArea, color)
      }

    }

    canvas.drawCustom(10, {

      Renderer.get.setBlend(Renderer.BlendPremultipliedAlpha)

      for {
        (entity, gui) <- nextWireGui
        (slot, index) <- gui.slots.zipWithIndex if !slot.isInput
        anotherSlot <- slot.connectedSlot
        anotherGui <- nextWireGui.get(anotherSlot.entity)
      } {
        val anotherIndex = anotherGui.slots.indexOf(anotherSlot)
        val posA = gui.slotCenters(index)
        val posB = anotherGui.slotCenters(anotherIndex)

        val width = globalRenderSystem.screenHeight * (8.0 / 720.0)

        val slotDir = if (slot.isInput) Vector2(-1.0, 0.0) else Vector2(1.0, 0.0)
        val diff = posB - posA
        val dist = diff.length

        val wrongSide = clamp(-(diff dot slotDir) / width * 0.3 + 1.0, 0.0, 2.0)
        val endStrength = wrongSide * width * 10.0

        val beginDir = Vector2(slotDir.x, wrongSide * 0.1)
        val endDir = Vector2(slotDir.x, -wrongSide * 0.1)

        val nodes = Seq(
          HermiteNode(posA, beginDir * (dist + endStrength)),
          HermiteNode(posB, endDir * (dist + endStrength)),
        )

        lineBatch.drawHermite(WireSprite, nodes, width, width * 0.5, width * 10.0, 10.0)
      }

      for (active <- activeSlot) {
        val slot = active.gui.slots(active.slot)
        val center = active.gui.slotCenters(active.slot)
        val mouse = AppWindow.mousePosition

        val width = globalRenderSystem.screenHeight * (8.0 / 720.0)

        val slotDir = if (slot.isInput) Vector2(-1.0, 0.0) else Vector2(1.0, 0.0)
        val diff = mouse - center
        val dist = diff.length

        val wrongSide = clamp(-(diff dot slotDir) / width * 0.3 + 1.0, 0.0, 2.0)
        val endStrength = wrongSide * width * 10.0

        val beginDir = Vector2(slotDir.x, wrongSide * 0.1)
        val endDir = Vector2(slotDir.x, -wrongSide * 0.1)

        val nodes = Seq(
          HermiteNode(center, beginDir * (dist + endStrength)),
          HermiteNode(AppWindow.mousePosition, endDir * (dist + endStrength)),
        )

        lineBatch.drawHermite(WireSprite, nodes, width, width * 0.5, width * 10.0, 10.0)
      }

      for (slot <- activeHudSlot) {
        val projected = viewProjection.projectPoint(slot.worldPosition)
        val x = (projected.x + 1.0) * 0.5 * globalRenderSystem.screenWidth
        val y = (1.0 - (projected.y + 1.0) * 0.5) * globalRenderSystem.screenHeight
        val pos = Vector2(x, y)

        val width = globalRenderSystem.screenHeight * (8.0 / 720.0)
        val nodes = Seq(pos, AppWindow.mousePosition)

        lineBatch.draw(WireSprite, nodes, width)
      }

      lineBatch.flush()
    })

    prevWireGui = nextWireGui

    for (entity <- hoveredTower if entity.hasFlag(Flag_Slots)) {
      val slots = towerSystem.getSlots(entity)

      val clickIndex = hudSlotInput.clickIndex
      if (clickIndex >= 0 && hudSlotEntity == entity) {
        val clickedSlot = slots(clickIndex)

        activeHudSlot match {
          case Some(prevSlot) =>
            connectSlots(prevSlot, clickedSlot)
            activeHudSlot = None

          case None =>
            selectedTower = None
            activeHudSlot = Some(clickedSlot)
            tutorialSystem.progress(TutorialSystem.StartBuildCable, 1.0)
        }
      }

      val hoverT = math.min(towerHoverTime / 0.05, 1.0)
      val animT = (smoothStep(hoverT * 0.5 + 0.5) - 0.5) * 2.0

      var allowed = true

      for (name <- tutorialSystem.allowedCableTarget) {
        if (!entity.prototype.name.contains(name))
          allowed = false
      }

      if (allowed) {
        for ((slot, index) <- slots.zipWithIndex) {
          val worldPos = slot.worldPosition

          val projected = viewProjection.projectPoint(worldPos)
          val x = (projected.x + 1.0) * 0.5 * globalRenderSystem.screenWidth
          val y = (1.0 - (projected.y + 1.0) * 0.5) * globalRenderSystem.screenHeight
          val pos = Vector2(x, y)
          val extent = math.min(globalRenderSystem.screenWidth, globalRenderSystem.screenHeight) * 0.05
          val size = Vector2(extent, extent)
          val anchor = Vector2(0.5, 0.5)

          val layout = new Layout(Vector2.One, x - size.x * 0.5, y - size.y * 0.5, x + size.x * 0.5, y + size.y * 0.5)

          val baseAlpha = if (hudSlotInput.focusIndex == index) 0.9 else 0.5
          val alpha = baseAlpha * animT

          val isCompatbile = activeHudSlot.forall(_.isInput != slot.isInput)
          val isFull = slot.connection.isDefined

          val sprite = (isCompatbile, isFull) match {
            case (false, false) => SlotHudIncompatibleEmptySprite
            case (false, true ) => SlotHudIncompatibleFullSprite
            case (true, false) => SlotHudEmptySprite
            case (true, true ) => SlotHudFullSprite
          }

          canvas.draw(3, sprite, pos, size, anchor, Color.White.copy(a = alpha))
          inputs.add(3, hudSlotInput, layout, 0.0, index)
        }

        hudSlotEntity = entity
      }
    }
  }

  override def renderBuildGui(canvas: Canvas, viewProjection: Matrix4): Unit = {
    if (!pauseSystem.paused) return

    val leftPanel = Layout.screen.containSnapped(200.0, 200.0,
      snapScale = 1.0,
      magScale = 2.0,
      minScale = 4.0,
      anchor = Vector2(0.0, 0.0),
      relativeScale = 0.5,
    )

    leftPanel.padAround(10.0)

    val currentMoneyArea = leftPanel.pushTop(20.0)
    canvas.drawText(1, CurrentMoneyText, currentMoneyArea, persistent.currentMoney.toString)

    for {
      buildE <- buildEntity.orElse(selectedTower.map(_.prototype))
      buildC <- buildE.find(BuildableComponent)
    } {
      leftPanel.padTop(10.0)
      val titleArea = leftPanel.pushTop(24.0)
      canvas.drawText(1, ObjectNameText, titleArea, lc"${buildC.locale}.name")

      val priceArea = leftPanel.pushTop(20.0)
      val priceText = Locale.getExpression("game.buildPrice", "price" -> buildC.price.toString)
      canvas.drawText(1, ObjectPriceText, priceArea, priceText)

      for (desc <- Locale.getSimpleOption(s"${buildC.locale}.desc")) {
        leftPanel.padTop(5.0)
        val descArea = leftPanel
        canvas.drawTextWrapped(1, ObjectDescText, descArea, lc"${buildC.locale}.desc")
      }
    }

    for (blocker <- buildBlockerEntities) {
      val projected = viewProjection.projectPoint(blocker.position + Vector3(0.0, 3.0, 0.0))

      val x = (projected.x + 1.0) * 0.5 * globalRenderSystem.screenWidth
      val y = (1.0 - (projected.y + 1.0) * 0.5) * globalRenderSystem.screenHeight
      val pos = Vector2(x, y)
      val extent = math.min(globalRenderSystem.screenWidth, globalRenderSystem.screenHeight) * 0.1
      val size = Vector2(extent, extent)
      canvas.draw(0, BlockerSprite, pos, size, CenterAnchor, Color.White)
    }
  }

  override def renderIngameGui(viewProjection: Matrix4): Unit = {
    val renderer = Renderer.get

    renderer.setMode(DepthTest, BlendPremultipliedAlpha, CullNone)

    // Borrow the Canvas SpriteBatch
    val sb = Canvas.shared.get.spriteBatch
    val prevWvp = sb.worldViewProjection

    sb.worldViewProjection = Some(viewProjection * ingameGuiWorld)


    for ((min, max) <- tutorialSystem.buildArea) {
      if (buildEntity.isDefined) {
        val radius = 2.0
        val color = ForbiddenBuildAreaColor
        sb.draw(QuadSprite, Vector2(min.x - radius, min.y - radius), Vector2(radius, max.y - min.y + 2 * radius), color)
        sb.draw(QuadSprite, Vector2(max.x, min.y - radius), Vector2(radius, max.y - min.y + 2 * radius), color)
        sb.draw(QuadSprite, Vector2(min.x, min.y - radius), Vector2(max.x - min.x, radius), color)
        sb.draw(QuadSprite, Vector2(min.x, max.y), Vector2(max.x - min.x, radius), color)
      }
    }

    for (sel <- selectedTower) {
      val pos = sel.position
      val sd = new SpriteDraw()
      sd.sprite = TowerSelectSprite
      sd.m11 = 8.0f
      sd.m22 = 8.0f
      sd.m13 = pos.x.toFloat
      sd.m23 = pos.z.toFloat
      sd.anchorX = 0.5f
      sd.anchorY = 0.5f
      sb.draw(sd)
    }

    for (blocker <- buildBlockerEntities) {
      val pos = blocker.position
      val sd = new SpriteDraw()
      sd.sprite = IngameBlockerSprite
      sd.m11 = 8.0f
      sd.m22 = 8.0f
      sd.m13 = pos.x.toFloat
      sd.m23 = pos.z.toFloat
      sd.anchorX = 0.5f
      sd.anchorY = 0.5f
      sb.draw(sd)
    }

    sb.flush()

    sb.worldViewProjection = prevWvp
  }

  override def addMoney(amount: Int): Unit = {
    persistent.currentMoney += amount
  }

  override def unload(): Unit = {
    lineBatch.unload()
  }

  override def entitiesDeleted(entities: EntitySet): Unit = {
    for (entity <- entities.flag(Flag_GridOccupier)) {
      entity.clearFlag(Flag_GridOccupier)
      val (x, y, w, h) = entityToGridArea.remove(entity).get
      clearGridOccupied(x, y, w, h)
    }
  }
}

