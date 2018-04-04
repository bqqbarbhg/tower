package game.system

import scala.collection.mutable
import core._
import game.lighting.LightProbe
import render._
import render.Renderer.UniformRef

class GroundMesh {
  var lightProbes: Array[LightProbe] = _
  var vertexBuffer: VertexBuffer = _


}

class GroundSystem(val minX: Int, val minZ: Int, val maxX: Int, val maxZ: Int) {

  val TileSize = 16.0
  val InvTileSize = 1.0 / TileSize

  class GroundTile(val x: Int, val y: Int) {
    val posX = x * TileSize
    val posZ = y * TileSize

    val lightProbe: LightProbe = LightSystem.addStaticGroundProbe(Vector3(posX, 0.0, posZ)).probe
  }

  private val groundMap = new mutable.HashMap[Long, GroundTile]()

  def getTile(x: Int, y: Int): GroundTile = {
    val key = y.toLong << 32 | x.toLong
    groundMap.getOrElseUpdate(key, new GroundTile(x, y))
  }

  def getTileContaining(pos: Vector3): GroundTile = getTileContaining(pos.x, pos.z)
  def getTileContaining(x: Double, z: Double): GroundTile = {
    val ix = math.floor(x * InvTileSize).toInt
    val iy = math.floor(z * InvTileSize).toInt
    getTile(ix, iy)
  }

  def getProbesAndWeights(pos: Vector3, probes: Array[LightProbe], weights: Array[Double]): Unit =
    getProbesAndWeights(pos.x, pos.z, probes, weights)

  def getProbesAndWeights(x: Double, z: Double, probes: Array[LightProbe], weights: Array[Double]): Unit = {
    val t00 = getTileContaining(x, z)
    val t01 = getTile(t00.x + 1, t00.y)
    val t10 = getTile(t00.x, t00.y + 1)
    val t11 = getTile(t00.x + 1, t00.y + 1)

    val dx1 = (x - t00.posX) * InvTileSize
    val dz1 = (z - t00.posZ) * InvTileSize
    val dx0 = 1.0 - dx1
    val dz0 = 1.0 - dz1

    probes(0) = t00.lightProbe; weights(0) = dz0*dx0
    probes(1) = t01.lightProbe; weights(1) = dz0*dx1
    probes(2) = t10.lightProbe; weights(2) = dz1*dx0
    probes(3) = t11.lightProbe; weights(3) = dz1*dx1
  }

}

