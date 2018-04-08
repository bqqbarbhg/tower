package game.system.rendering

import core._
import game.shader._
import render._
import game.system._
import game.system.Entity._
import game.system.rendering.AmbientSystem.Probe
import game.system.rendering.GroundSystem._
import game.system.rendering.GroundSystemImpl._
import render.VertexBuffer
import util.geometry.Aabb

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object GroundSystem {

  class GroundPlate {
    var position: Vector3 = Vector3.Zero
    var aabb: Aabb = null
    var probes: Array[Probe] = null

    def draw(): Unit = {
      val impl = groundSystem.asInstanceOf[GroundSystemImpl]
      val renderer = Renderer.get

      val vb = impl.plateVertexBuffer
      val ib = impl.plateIndexBuffer

      renderer.pushUniform(GroundPlateUniform, u => {
        GroundPlateUniform.Position.set(u, position, 0.0f)
      })

      renderer.pushUniform(LightProbeUniform, u => {
        LightProbeUniform.write(u, probes)
      })

      renderer.drawElements(ib.numIndices, ib, vb)
    }
  }

}

trait GroundSystem {

  /** Initialization: Create entities for the ground plates */
  def createGroundPlates(): Unit

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

  /** Gather a list of ground plates from a visible entities */
  def collectGroundPlates(visible: EntitySet): ArrayBuffer[GroundPlate]
}

object GroundSystemImpl {

  val GroundSpec = {
    import render.VertexSpec._
    import render.VertexSpec.DataFmt._
    VertexSpec(Vector(
      Attrib(2, F32, Identifier("Position")),
      Attrib(4, UI8, Identifier("ProbeIndex")),
      Attrib(4, UN8, Identifier("ProbeWeight")),
    ))
  }

}

class GroundSystemImpl extends GroundSystem {
  val TileSize = 10.0
  val InvTileSize = 1.0 / TileSize

  val GridWidth = 3 * 16
  val GridHeight = 3 * 16

  val OffsetX = GridWidth / 2 * -TileSize
  val OffsetZ = GridHeight / 2 * -TileSize

  val GroundResolution = 8

  val probes = (for {
    z <- 0 until (GridHeight + 1)
    x <- 0 until (GridWidth + 1)
  } yield {
    val pos = Vector3(OffsetX + x * TileSize, 0.0, OffsetZ + z * TileSize)
    ambientSystem.createGlobalProbeForIndirect(pos)
  }).toArray

  val plateVertexBuffer: VertexBuffer = createPlateVertexBuffer(3, 3)
  val plateIndexBuffer: IndexBuffer = createPlateIndexBuffer(3, 3)

  def createPlateVertexBuffer(numX: Int, numZ: Int): VertexBuffer = {
    val res = GroundResolution

    val vertsPerChunk = (res + 1) * (res + 1)
    val vertsTotal = vertsPerChunk * numX * numZ
    val groundVertSize = GroundSpec.sizeInBytes * vertsTotal
    val groundVerts = Memory.alloc(groundVertSize)

    val invRes = 1.0 / res.toDouble

    for {
      chunkZ <- 0 until numZ
      chunkX <- 0 until numX
    } {
      val baseVert = (chunkZ * numX + chunkX) * vertsPerChunk

      val x0 = (chunkX + 0) * TileSize
      val x1 = (chunkX + 1) * TileSize
      val z0 = (chunkZ + 0) * TileSize
      val z1 = (chunkZ + 1) * TileSize

      val probeA = ((chunkZ + 0) * (numX + 1) + (chunkX + 0))
      val probeB = ((chunkZ + 0) * (numX + 1) + (chunkX + 1))
      val probeC = ((chunkZ + 1) * (numX + 1) + (chunkX + 0))
      val probeD = ((chunkZ + 1) * (numX + 1) + (chunkX + 1))

      val probeI = probeA | probeB << 8 | probeC << 16 | probeD << 24

      var vertZ = 0
      var dz = 0.0
      while (vertZ <= res) {
        val b = groundVerts
        b.position((baseVert + vertZ * (res + 1)) * GroundSpec.sizeInBytes)

        val zf = if (vertZ == res)
          z1.toFloat
        else
          (z0 * (1.0 - dz) + z1 * dz).toFloat

        var vertX = 0
        var dx = 0.0
        while (vertX <= res) {

          if (vertX == res)
            b.putFloat(x1.toFloat)
          else
            b.putFloat((x0 * (1.0 - dx) + x1 * dx).toFloat)
          b.putFloat(zf)

          b.putInt(probeI)

          val nx = dx
          val ny = dz
          val px = 1.0 - nx
          val py = 1.0 - ny

          b.put((px * py * 255.0).toByte)
          b.put((nx * py * 255.0).toByte)
          b.put((px * ny * 255.0).toByte)
          b.put((nx * ny * 255.0).toByte)

          vertX += 1
          dx += invRes
        }
        vertZ += 1
        dz += invRes
      }
    }

    groundVerts.position(0)
    val vb = VertexBuffer.createStatic(GroundSpec, groundVerts)
    Memory.free(groundVerts)
    vb.withLabel("Ground plate verts")
  }

  def createPlateIndexBuffer(numX: Int, numY: Int): IndexBuffer = {
    val res = GroundResolution

    val numQuads = numX * numY * res * res
    val groundIndices = Memory.alloc(numQuads * 6 * 2)
    val vertsPerChunk = (res + 1) * (res + 1)

    for {
    chunkY <- 0 until numY
    chunkX <- 0 until numX
    } {
      val baseVert = (chunkY * numX + chunkX) * vertsPerChunk

      var vertY = 0
      while (vertY < res) {
      var base = baseVert + vertY * (res + 1)

      var vertX = 0
      while (vertX < res) {
        val a = base.toShort
        val b = (base + 1).toShort
        val c = (base + (res + 1)).toShort
        val d = (base + 1 + (res + 1)).toShort

        groundIndices.putShort(c)
        groundIndices.putShort(b)
        groundIndices.putShort(a)
        groundIndices.putShort(b)
        groundIndices.putShort(c)
        groundIndices.putShort(d)

        base += 1
        vertX += 1
      }
      vertY += 1
    }

  }

  groundIndices.position(0)

  val ib = IndexBuffer.createStatic(groundIndices)
  Memory.free(groundIndices)
  ib.withLabel("Ground plate indices")
}

  val entityToPlate = new mutable.HashMap[Entity, GroundPlate]()

  def getProbe(x: Int, z: Int): Probe = probes(z * (GridWidth + 1) + x)

  def createGroundPlates(): Unit = {
    val Name = "GroundPlate"

    for {
      z <- 0 until GridHeight / 3
      x <- 0 until GridWidth / 3
    } {
      val cx = x * 3
      val cz = z * 3

      val base = Vector3(OffsetX + cx * TileSize, 0.0, OffsetZ + cz * TileSize)
      val min = base + Vector3(0.0, -0.2, 0.0)
      val max = base + Vector3(3 * TileSize, +0.2, 3 * TileSize)

      val plate = new GroundPlate()
      plate.position = base
      plate.aabb = Aabb.fromMinMax(min, max)
      plate.probes = new Array[Probe](16)

      val entity = new Entity(true, Name)

      for {
        iz <- 0 to 3
        ix <- 0 to 3
      } {
        val index = iz * 4 + ix
        val probe = getProbe(cx + ix, cz + iz)
        plate.probes(index) = probe

        ambientSystem.addProbeDependency(entity, probe)
      }

      cullingSystem.addAabb(entity, plate.aabb, CullingSystem.MaskRender)

      entity.setFlag(Flag_GroundPlate)
      entityToPlate(entity) = plate
    }

  }

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

  override def collectGroundPlates(visible: EntitySet): ArrayBuffer[GroundPlate] = {
    visible.flag(Flag_GroundPlate).map(entityToPlate)
  }

}

