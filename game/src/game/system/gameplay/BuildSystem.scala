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
import locale.Locale
import locale.LocaleString._
import platform.{AppWindow, KeyEvent}
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
  )

}

sealed trait BuildSystem {

  /** Set the entity to present to the user as buildable.
    * May be called multiple times with the same entity. */
  def setEntityTypeToBuild(entityType: Option[EntityType])

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

  /** Release resources used by the system */
  def unload(): Unit
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

  class WireGui(val entity: Entity) {
    var slots: Seq[Slot] = NoSlots
    var slotCenters: Array[Vector2] = NoCenters
    val iconInput = new InputArea()
  }

  case class SlotRef(gui: WireGui, slot: Int)

  val GridSize = 4.0
}

final class BuildSystemImpl extends BuildSystem {
  var buildEntity: Option[EntityType] = None
  var buildPreview: Option[BuildPreview] = None
  var prevMouseDown: Boolean = false
  var buildBlockerEntities = new ArrayBuffer[Entity]()
  var rayCastResult = new ArrayBuffer[RayHit]()
  var failCooldown: Double = 0.0
  var prevWireGui = mutable.HashMap[Entity, WireGui]()
  var wireGuiEnabled = false
  var selectedTower: Option[Entity] = None

  val deleteBind = KeyEvent.NameToKey.get(Options.current.binds.delete).getOrElse(KeyEvent.Delete)

  val ingameGuiWorld = Matrix43.world(Vector3(1.0, 0.0, 0.0), Vector3(0.0, 0.0, 1.0), Vector3(0.0, 1.0, 0.0), Vector3(0.0, 2.0, 0.0))

  var activeSlot: Option[SlotRef] = None

  val lineBatch = new LineBatch()

  val QuadSprite = Identifier("gui/menu/background_white.png")

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

  override def update(dt: Double, invViewProj: Matrix4, inputs: InputSet): Unit = {
    val mouseDown = AppWindow.mouseButtonDown(0)
    val clicked = mouseDown && !prevMouseDown && inputs.focusedLayer < -1000

    if (AppWindow.mouseButtonDown(1)) {
      activeSlot = None
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
      selectedTower = None

      val t = ray.intersect(GroundPlane)
      var groundPoint = t.map(ray.point).map(point => {
        val roundX = (math.floor(point.x / GridSize) + 0.0) * GridSize + GridSize * 0.5
        val roundZ = (math.floor(point.z / GridSize) + 0.0) * GridSize + GridSize * 0.5
        Vector3(roundX, 0.0, roundZ)
      })

      if (inputs.focusedLayer >= -1000)
        groundPoint = None

      var validPlace = false

      for (point <- groundPoint) {
        val bounds = Aabb(point, Vector3(1.0, 5.0, 1.0))
        cullingSystem.queryAabb(bounds, MaxBlockers, CullingSystem.MaskGameplay, buildBlockerEntities)
        validPlace = buildBlockerEntities.isEmpty

        if (clicked) {
          if (validPlace) {
            entitySystem.create(buildE, point)

            for (buildC <- buildE.find(BuildableComponent)) {
              val sound = SoundAsset(buildC.placeSound)
              audioSystem.play(sound, AudioSystem.Sfx)
            }
          } else {
            if (failCooldown <= 0.0) {
              audioSystem.play(FailPlaceSound, AudioSystem.Ui)
              failCooldown = 0.25
            }
          }
        }
      }

      for (preview <- buildPreview) {
        for (point <- groundPoint)
          preview.entity.position = point + Vector3(0.0, 0.01, 0.0)
        preview.time += dt
        preview.valid = validPlace
        preview.visible = groundPoint.nonEmpty
      }
    }

    if (clicked)
      selectedTower = None

    if (buildEntity.isEmpty) {
      rayCastResult.clear()
      cullingSystem.queryRay(ray, MaxRayCast, CullingSystem.MaskGameplay, rayCastResult)
      val towers = rayCastResult.iterator.filter(_.entity.hasFlag(Flag_Tower))
      if (towers.nonEmpty) {
        val closest = towers.minBy(_.t)

        if (clicked) {
          selectedTower = Some(closest.entity)
        }
      }
    }

    for (selected <- selectedTower) {

      if (AppWindow.keyDownEvents.exists(_.key == deleteBind)) {
        audioSystem.play(BreakSound, AudioSystem.Sfx)
        selected.delete()
        selectedTower = None
      }

    }

    prevMouseDown = mouseDown
  }

  override def renderPreview(): Unit = {
    val renderer = Renderer.get

    renderer.setDepthMode(false, true)
    renderer.setBlend(Renderer.BlendAddAlpha)

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

  override def renderWireGui(canvas: Canvas, inputs: InputSet, visible: EntitySet, viewProjection: Matrix4): Unit = {
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
    for (selected <- selectedTower if selected.hasFlag(Flag_Slots)) {
      if (!nextWireGui.contains(selected)) {
        makeWireGui(selected)
      }
    }

    for ((entity, wireGui) <- nextWireGui) {
      val clickIndex = wireGui.iconInput.clickIndex
      if (clickIndex >= 0) {
        activeSlot match {
          case Some(slot) =>
            towerSystem.connectSlots(slot.gui.slots(slot.slot), wireGui.slots(clickIndex))
            activeSlot = None
            selectedTower = None

          case None =>
            activeSlot = Some(SlotRef(wireGui, clickIndex))
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

        val text = lc"${slot.locale}.name"

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

      lineBatch.flush()
    })

    prevWireGui = nextWireGui
  }

  override def renderBuildGui(canvas: Canvas, viewProjection: Matrix4): Unit = {
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

    renderer.setCull(false)
    renderer.setDepthMode(false, true)
    renderer.setBlend(Renderer.BlendPremultipliedAlpha)

    // Borrow the Canvas SpriteBatch
    val sb = Canvas.shared.get.spriteBatch
    val prevWvp = sb.worldViewProjection

    sb.worldViewProjection = Some(viewProjection * ingameGuiWorld)

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

  override def unload(): Unit = {
    lineBatch.unload()
  }
}

