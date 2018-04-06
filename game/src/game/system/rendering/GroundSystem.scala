package game.system.rendering

import core._
import game.system.rendering.AmbientSystem.Probe

trait GroundSystem {

  /** Get closest probes to `pos`. See other overload for more info. */
  def getProbesAndWeights(pos: Vector3, probes: Array[Probe], weights: Array[Double]): Unit =
    getProbesAndWeights(pos.x, pos.z, probes, weights)

  /**
    * Get the closest four ground ambient probes and relative weights to point
    * `Vector3(x, 0, z)`. Used for bilinearly interpolating ground ambient.
    *
    * @param probes Array with capacity for resulting 4 probes.
    * @param weights Array with capacity for resulting 4 weights.
    */
  def getProbesAndWeights(x: Double, z: Double, probes: Array[Probe], weights: Array[Double]): Unit
}



