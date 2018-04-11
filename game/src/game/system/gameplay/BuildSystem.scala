package game.system.gameplay


import scala.collection.mutable
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
import platform.AppWindow
import ui.{Canvas, InputSet}
import util.geometry._

import scala.collection.mutable.ArrayBuffer

object BuildSystem {

  val Assets = new AssetBundle("BuildSystem",
    PlaceMeshShader,
    NoiseTex,
    HudAtlas,
    FailPlaceSound,
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

  /** Render GUI elements used by this system */
  def renderBuildGui(canvas: Canvas, viewProjection: Matrix4): Unit

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

  val HudAtlas = AtlasAsset("atlas/hud.s2at")
  val FailPlaceSound = SoundAsset("audio/effect/error.wav.s2au")

  val PreviewValidColor = Color.rgba(0xFFFFFF, 1.0)
  val PreviewBadColor = Color.rgba(0xFF0000, 1.0)

  val BlockerSprite = Identifier("gui/hud/build_blocker.png")
  val CenterAnchor = Vector2(0.5, 0.5)
}

final class BuildSystemImpl extends BuildSystem {
  var buildEntity: Option[EntityType] = None
  var buildPreview: Option[BuildPreview] = None
  var prevMouseDown: Boolean = false
  var buildBlockerEntities = new ArrayBuffer[Entity]()
  var failCooldown: Double = 0.0

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
    val clicked = mouseDown && !prevMouseDown

    buildBlockerEntities.clear()
    if (failCooldown > 0.0) {
      failCooldown -= dt
    }

    for (buildE <- buildEntity) {
      val mousePos = AppWindow.mousePosition
      val relX = mousePos.x / globalRenderSystem.screenWidth
      val relY = mousePos.y / globalRenderSystem.screenHeight
      val ndcX = relX * 2.0 - 1.0
      val ndcY = -(relY * 2.0 - 1.0)

      val near = invViewProj.projectPoint(Vector3(ndcX, ndcY, -1.0))
      val far = invViewProj.projectPoint(Vector3(ndcX, ndcY, +1.0))

      val ray = Ray.fromUnnormalized(near, far - near)
      val t = ray.intersect(GroundPlane)
      var groundPoint = t.map(ray.point)

      if (inputs.focusedLayer >= -1000)
        groundPoint = None

      var validPlace = false

      for (point <- groundPoint) {
        val bounds = Aabb(point, Vector3(6.0, 5.0, 6.0))
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
}

