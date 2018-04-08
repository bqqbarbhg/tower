package game.system.rendering

import core._
import game.system.rendering.AmbientSystem.Probe

object GroundSystem {
}

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

class GroundSystemImpl extends GroundSystem {
  val TileSize = 10.0
  val InvTileSize = 1.0 / TileSize

  val GridWidth = 64
  val GridHeight = 64

  val OffsetX = GridWidth / 2 * -TileSize
  val OffsetZ = GridHeight / 2 * -TileSize

  val probes = (for {
    z <- 0 until (GridHeight + 1)
    x <- 0 until (GridWidth + 1)
  } yield {
    val pos = Vector3(OffsetX + x * TileSize, 0.0, OffsetZ + z * TileSize)
    ambientSystem.createGlobalProbeForIndirect(pos)
  }).toArray

  def getProbe(x: Int, y: Int): Probe = probes(y * (GridWidth + 1) + x)

  override def getProbesAndWeights(x: Double, z: Double, probes: Array[Probe], weights: Array[Double]): Unit = {
    val cx = clamp((x - OffsetX) * InvTileSize, 0.0, GridWidth.toDouble - 0.001)
    val cz = clamp((z - OffsetZ) * InvTileSize, 0.0, GridHeight.toDouble - 0.001)

    val ix = cx.toInt
    val iz = cz.toInt

    assert(ix >= 0 && ix < GridWidth)
    assert(iz >= 0 && iz < GridHeight)

    probes(0) = getProbe(ix + 0, iz + 0)
    probes(1) = getProbe(ix + 1, iz + 0)
    probes(2) = getProbe(ix + 0, iz + 1)
    probes(3) = getProbe(ix + 1, iz + 1)

    val nx = cx - ix.toDouble
    val nz = cz - iz.toDouble
    val px = 1.0 - nx
    val pz = 1.0 - nz
    weights(0) = px * pz
    weights(1) = nx * pz
    weights(2) = px * nz
    weights(3) = nx * nz
  }

}

