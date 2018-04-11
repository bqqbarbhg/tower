package game.system.gameplay


import scala.collection.mutable

import core._
import render._
import asset._
import game.component._
import game.system._
import game.system.Entity._
import game.system.base._
import game.system.rendering._
import game.system.rendering.ModelSystem._
import game.shader.PlaceMeshShader
import BuildSystemImpl._
import core.Matrix4
import platform.AppWindow
import util.geometry._

object BuildSystem {

  val Assets = new AssetBundle("BuildSystem",
    PlaceMeshShader,
    NoiseTex,
  )

}

sealed trait BuildSystem {

  /** Set the entity to present to the user as buildable.
    * May be called multiple times with the same entity. */
  def setEntityTypeToBuild(entityType: Option[EntityType])

  /** Tick the system */
  def update(dt: Double, invViewProj: Matrix4): Unit

  /** Render build preview */
  def renderPreview(): Unit

}

object BuildSystemImpl {

  val NoiseTex = TextureAsset("effect/noise.png.s2tx")

  class BuildPreview(val entityType: EntityType, val entity: Entity, val comp: BuildPreviewComponent) {
    val model = ModelAsset(comp.model)
    val mask = TextureAsset(comp.mask)
    var time: Double = 0.0
    var modelInst: ModelInstance = _

    def delete(): Unit = {
      entity.delete()
    }
  }

  val GroundPlane = Plane(Vector3.Up, 0.0)

}

final class BuildSystemImpl extends BuildSystem {
  var buildEntity: Option[EntityType] = None
  var buildPreview: Option[BuildPreview] = None
  var prevMouseDown: Boolean = false

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

  override def update(dt: Double, invViewProj: Matrix4): Unit = {
    val mouseDown = AppWindow.mouseButtonDown(0)
    val clicked = mouseDown && !prevMouseDown

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
      val groundPoint = t.map(ray.point)

      for (point <- groundPoint) {
        if (clicked) {
          entitySystem.create(buildE, point)
        }
      }

      for (preview <- buildPreview) {
        for (point <- groundPoint)
          preview.entity.position = point + Vector3(0.0, 0.01, 0.0)
        preview.time += dt
      }
    }

    prevMouseDown = mouseDown
  }

  override def renderPreview(): Unit = {
    val renderer = Renderer.get

    renderer.setDepthMode(false, true)
    renderer.setBlend(Renderer.BlendAddAlpha)

    for (preview <- buildPreview) {
      val models = modelSystem.collectModels(preview.entity)
      modelSystem.updateModels(models)
      val meshes = modelSystem.collectMeshInstances(models)

      PlaceMeshShader.get.use()
      renderer.setTexture(PlaceMeshShader.Textures.Noise, NoiseTex.get.texture)

      for {
        (mesh, insts) <- meshes.meshes
        inst <- insts
      } {
        renderer.setTexture(PlaceMeshShader.Textures.Placemask, preview.mask.get.texture)

        renderer.pushUniform(PlaceMeshShader.PixelUniform, u => {
          import PlaceMeshShader.PixelUniform._
          val time = preview.time.toFloat
          UvOffset.set(u, time, 0.0f, 0.0f, 0.0f)
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

}

