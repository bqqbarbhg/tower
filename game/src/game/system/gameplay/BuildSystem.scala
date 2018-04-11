package game.system.gameplay

import render._
import asset._

import scala.collection.mutable
import game.component._
import game.system._
import game.system.Entity._
import game.system.rendering._
import game.system.rendering.ModelSystem._
import game.shader.PlaceMeshShader
import BuildSystemImpl._
import platform.AppWindow

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
  def update(dt: Double): Unit

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

}

final class BuildSystemImpl extends BuildSystem {
  var buildPreview: Option[BuildPreview] = None

  def makePreview(entityType: EntityType): Option[BuildPreview] = {
    for (comp <- entityType.find(BuildPreviewComponent)) yield {
      val entity = new Entity(false, "Build preview")
      val pre = new BuildPreview(entityType, entity, comp)
      pre.modelInst = modelSystem.addModel(pre.entity, pre.model)
      pre
    }
  }

  override def setEntityTypeToBuild(entityType: Option[EntityType]): Unit = {
    entityType match {
      case Some(et) =>
        buildPreview match {
          case Some(pre) =>
            if (pre.entityType != et) {
              pre.delete()
              buildPreview = makePreview(et)
            }
          case None =>
            buildPreview = makePreview(et)
        }

      case None =>
        for (pre <- buildPreview) pre.delete()
        buildPreview = None
    }
  }

  override def update(dt: Double): Unit = {
    for (preview <- buildPreview) {
      preview.time += dt
    }
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

