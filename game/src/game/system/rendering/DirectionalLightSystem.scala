package game.system.rendering

import scala.collection.mutable.ArrayBuffer

import core._
import AmbientSystem.Probe

sealed trait DirectionalLightSystem {

  /**
    * Set the global directional light
    */
  def setLight(direction: Vector3, intensity: Vector3): Unit

  /**
    * Add point-light to visible total ambient probes.
    */
  def updateVisibleProbes(probes: ArrayBuffer[Probe]): Unit

}

final class DirectionalLightSystemImpl extends DirectionalLightSystem {

  var lightDirection: Vector3 = Vector3.Zero
  var lightIntensity: Vector3 = Vector3.Zero

  override def setLight(direction: Vector3, intensity: Vector3): Unit = {
    lightDirection = direction
    lightIntensity = intensity
  }

  override def updateVisibleProbes(probes: ArrayBuffer[Probe]): Unit = {
    var ix = 0
    val len = probes.length
    while (ix < len) {
      val probe = probes(ix)
      assert(probe.gatherTotal, "Directional probes must gather all light")

      probe.totalIrradianceProbe.addDirectional(lightDirection, lightIntensity)

      ix += 1
    }
  }
}

