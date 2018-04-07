package game.system.rendering

import scala.collection.mutable.ArrayBuffer
import core._
import game.system.Entity
import game.system.rendering.AmbientSystem.Probe
import game.system.rendering.ModelSystem.{MeshInstanceCollection, ModelInstance}
import task.{Scheduler, Task}
import util.geometry._

object WorldRenderSystem {

}

sealed trait WorldRenderSystem {
}

final class WorldRenderSystemImpl extends WorldRenderSystem {
  def renderWorld(s: Scheduler, viewProjection: Matrix4): Unit = {

    val frustum = Frustum.fromViewProjection(viewProjection)

    var visibleRender: Array[Entity] = null
    var visibleProbes: ArrayBuffer[Probe] = null
    var visibleModels: ArrayBuffer[ModelInstance] = null
    var visibleMeshes: MeshInstanceCollection = null
    var forwardDraws: ForwardRenderingSystem.Draws = null

    var visibleShadow: Array[Entity] = null
    var shadowModels: ArrayBuffer[ModelInstance] = null
    var shadowMeshes: MeshInstanceCollection = null
    var shadowDraws: ShadowRenderingSystem.Draws = null

    s.add("Cull view")(cullingSystem, visibleRender)() {
      visibleRender = cullingSystem.cullEntities(frustum, CullingSystem.MaskRender)
    }

    s.add("Cull shadow")(cullingSystem, visibleShadow)() {
      visibleShadow = cullingSystem.cullEntities(frustum, CullingSystem.MaskShadow)
    }

    s.add("Collect shadow models")(shadowModels)(visibleShadow) {
      shadowModels = modelSystem.collectVisibleModels(visibleRender)
    }

    s.add("Update shadow models")(shadowModels)(modelSystem) {
      modelSystem.updateModels(shadowModels)
    }

    s.add("Create shadow instances")(shadowMeshes)(modelSystem, shadowModels) {
      visibleMeshes = modelSystem.collectMeshInstances(shadowModels)
    }

    s.addTo("Create shadow draws")(Task.Main)(shadowRenderingSystem, shadowDraws)(modelSystem, shadowMeshes) {
      shadowDraws = shadowRenderingSystem.createMeshDraws(shadowMeshes)
    }

    s.add("Ambient probe visibility")(ambientSystem, visibleProbes)(visibleRender) {
      ambientSystem.updateVisibleProbes(visibleRender)
      visibleProbes = ambientSystem.currentlyVisibleProbes
    }

    s.add("Ambient dynamic point lights")(ambientPointLightSystem)() {
      ambientPointLightSystem.updateDynamicLights()
    }

    s.add("Ambient probe point lights")(ambientPointLightSystem, visibleProbes)() {
      ambientPointLightSystem.updateVisibleProbes(visibleProbes)
    }

    s.add("Ambient probe indirect light")(ambientSystem, visibleProbes)() {
      ambientSystem.updateIndirectLight(visibleProbes)
    }

    s.add("Collect visible models")(visibleModels)(modelSystem, visibleRender) {
      visibleModels = modelSystem.collectVisibleModels(visibleRender)
    }

    s.add("Update visible models")(visibleModels)(modelSystem) {
      modelSystem.updateModels(visibleModels)
    }

    s.add("Create model instances")(visibleMeshes)(modelSystem, visibleModels) {
      visibleMeshes = modelSystem.collectMeshInstances(visibleModels)
    }

    s.addTo("Create mesh draws")(Task.Main)(forwardRenderingSystem, forwardDraws)(visibleMeshes) {
      forwardDraws = forwardRenderingSystem.createMeshDraws(visibleMeshes)
    }

    s.add("Ambient point light cleanup")(ambientPointLightSystem)() {
      ambientPointLightSystem.frameCleanup()
    }

    s.add("Ambient cleanup")(ambientSystem)() {
      ambientSystem.frameCleanup()
    }

    s.add("Model cleanup")(modelSystem)() {
      modelSystem.frameCleanup()
    }

  }
}

