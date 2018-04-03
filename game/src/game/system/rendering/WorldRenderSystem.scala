package game.system.rendering

import core._
import util.geometry._

object WorldRenderSystem {

}

sealed trait WorldRenderSystem {
  def renderWorld(viewProjection: Matrix4): Unit
}

final class WorldRenderSystemImpl extends WorldRenderSystem {
  def renderWorld(viewProjection: Matrix4): Unit = {

    val frustum = Frustum.fromViewProjection(viewProjection)

    val visibleForRendering = cullingSystem.cullEntities(frustum, CullingSystem.MaskRender)
    val visibleForLighting = cullingSystem.cullEntities(frustum, CullingSystem.MaskLight)

    pointLightSystem.updateLightReceivers(visibleForRendering, visibleForLighting)

  }
}

