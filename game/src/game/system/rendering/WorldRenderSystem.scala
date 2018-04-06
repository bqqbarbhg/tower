package game.system.rendering

import scala.collection.mutable.ArrayBuffer

import core._
import game.system.Entity
import game.system.rendering.AmbientSystem.Probe
import task.Scheduler
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

    s.add("Cull view")(cullingSystem, visibleRender)() {
      visibleRender = cullingSystem.cullEntities(frustum, CullingSystem.MaskRender)
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

    s.add("Ambient point light cleanup")(ambientPointLightSystem)() {
      ambientPointLightSystem.frameCleanup()
    }

    s.add("Ambient cleanup")(ambientSystem)() {
      ambientSystem.frameCleanup()
    }

  }
}

