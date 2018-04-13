package game.system.rendering

import asset.ModelAsset
import core._
import render._
import game.shader._
import game.options.Options
import game.system._
import game.system.base._
import game.system.Entity._
import game.system.rendering.CableRenderSystem._
import game.system.rendering.CableRenderSystemImpl._
import game.system.rendering.AmbientSystem.Probe
import gfx.Model
import util.geometry.Aabb
import util.BinarySearch

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object CableRenderSystem {
  class CableMeshPart(val mesh: CableMesh) {

    var lightProbes: Array[Probe] = null
    var vertexBuffer: VertexBuffer = null
    var numRings: Int = 0
    var aabb: Aabb = null

    def draw(): Unit = {
      val impl = cableRenderSystem.asInstanceOf[CableRenderSystemImpl]
      val renderer = Renderer.get
      var baseRing = 0

      renderer.pushUniform(LightProbeUniform, u => LightProbeUniform.write(u, lightProbes))

      val maxRing = numRings - 1
      while (baseRing < maxRing) {
        val toDraw = math.min(maxRing - baseRing, impl.MaxRingsPerDraw)
        val quadsToDraw = impl.VertsPerRing * toDraw
        val baseVertex = baseRing * impl.VertsPerRing
        renderer.drawElements(quadsToDraw * 6, impl.indexBuffer, vertexBuffer, baseVertex = baseVertex)
        baseRing += toDraw
      }
    }

    def unload(): Unit = {
      vertexBuffer.free()
    }
  }

  class CableMesh {
    var numRings: Int = 0
    var length: Double = 0.0
    var parts: Array[CableMeshPart] = null

    def draw(): Unit = {
      for (part <- parts) {
        part.draw()
      }
    }
  }

  object CableNode {
    def apply(position: Vector3, tangent: Vector3): CableNode = CableNode(position, tangent, tangent)
  }

  /**
    * Represents a single node in the Hermite-interpolation based cable curve.
    *
    * @param position Position of the node
    * @param tangentIn Incoming tangent direction and magnitude
    * @param tangentOut Outgoing tangent direction and magnitude
    */
  case class CableNode(position: Vector3, tangentIn: Vector3, tangentOut: Vector3) {
    def this(position: Vector3, tangent: Vector3) = this(position, tangent, tangent)
  }

}

trait CableRenderSystem extends EntityDeleteListener {

  /** Create a new cable from a set of nodes. */
  def createCable(entity: Entity, nodes: Seq[CableNode], radius: Double): Aabb

  /** Collect all the cables from a set of visible entities. */
  def collectCableMeshes(visible: EntitySet): ArrayBuffer[CableMeshPart]

  /** Retrieve a list of alternative cable paths for a model */
  def getCablePathsForModel(asset: ModelAsset): Option[Array[Array[CableNode]]]

  /** Free used resources */
  def unload(): Unit

}

object CableRenderSystemImpl {

  val CableSpec = {
    import render.VertexSpec.Attrib
    import render.VertexSpec.DataFmt._
    VertexSpec(Vector(
      Attrib(3, F32, Identifier("Position")),
      Attrib(1, F32, Identifier("Distance")),
      Attrib(3, F32, Identifier("Normal")),
      Attrib(4, UI8, Identifier("ProbeIndex")),
      Attrib(4, UN8, Identifier("ProbeWeight")),
    ))
  }

  val CablePartName = "CablePart"

  case class CachedModelCables(model: Model, cables: Option[Array[Array[CableNode]]])
}

class CableRenderSystemImpl extends CableRenderSystem {

  val Quality = Options.current.graphics.quality.modelQuality

  val VertsPerRing = Quality match {
    case 0|1 => 4
    case 2 => 6
    case 3 => 8
  }
  val MaxRingsPerDraw = 128
  val PartMaxRings = 128

  val indexBuffer = {
    val data = Memory.alloc(VertsPerRing * MaxRingsPerDraw * 6 * 2)

    var ringIx = 0
    while (ringIx < MaxRingsPerDraw) {

      var quadIx = 0
      while (quadIx < VertsPerRing) {
        val prev = ringIx * VertsPerRing + quadIx
        val next = ringIx * VertsPerRing + (quadIx + 1) % VertsPerRing

        val a = prev.toShort
        val b = next.toShort
        val c = (prev + VertsPerRing).toShort
        val d = (next + VertsPerRing).toShort

        data.putShort(a)
        data.putShort(b)
        data.putShort(c)
        data.putShort(d)
        data.putShort(c)
        data.putShort(b)

        quadIx += 1
      }

      ringIx += 1
    }

    data.finish()
    val indexBuffer = IndexBuffer.createStatic(data)
    Memory.free(data)
    indexBuffer.withLabel("CableRenderSsytem.indexBuffer")
  }

  val entityToCablePart = new mutable.HashMap[Entity, CableMeshPart]()
  val modelCableCache = new mutable.HashMap[ModelAsset, CachedModelCables]()

  def getCablePathsForModel(asset: ModelAsset): Option[Array[Array[CableNode]]] = {
    val model = asset.getShallowUnsafe
    val cables = modelCableCache.get(asset).filter(_.model == model)
    cables match {
      case Some(c) => c.cables
      case None =>
        val cables = createCablePathsForModel(model)
        modelCableCache(asset) = CachedModelCables(model, cables)
        cables
    }
  }

  def createCablePathsForModel(model: Model): Option[Array[Array[CableNode]]] = {
    val allNodes = mutable.ArrayBuilder.make[Array[CableNode]]()

    val childrenForName = mutable.HashMap[Identifier, Vector[Identifier]]().withDefaultValue(Vector[Identifier]())
    val roots = ArrayBuffer[Identifier]()

    def getPathRecursive(head: Vector[CableNode], name: Identifier): Unit = {
      val node = model.findNodeByName(name)
      assert(node >= 0, s"Node not found for cable node '${name.toString}'")
      val children = childrenForName(name)
      val worldTransform = model.transformToRoot(node)
      val pos = worldTransform.translation
      val dir = worldTransform.forward * 2.0
      val self = CableNode(pos, dir)
      val list = head :+ self

      if (children.nonEmpty) {
        for (child <- children)
          getPathRecursive(list, child)
      } else {
        allNodes += list.toArray
      }
    }

    val start = model.findNodeByName(Identifier("Cables"))
    if (start >= 0) {
      val cableNodes = model.getChildNodes(start)
      val numberRegex = "^(.*)\\.(\\d{3})$".r

      val names = cableNodes.map(node => new Identifier(model.nodeName(node)).toString)

      for (name <- names) {
        val nameId = Identifier(name)
        name match {
          case numberRegex(prefix, numberStr) =>
            val number = numberStr.toInt
            val parent = if (number > 1) {
              val parentNumber = number - 1
              f"$prefix.${parentNumber}%03d"
            } else {
              prefix
            }

            val parentId = Identifier(parent)
            val prev = childrenForName(parentId)
            childrenForName(parentId) = prev :+ nameId
          case _ =>

            names.find(parent => name.startsWith(parent) && name != parent) match {
              case Some(parentName) =>
                val parentId = Identifier(parentName)
                val prev = childrenForName(parentId)
                childrenForName(parentId) = prev :+ nameId
              case None =>
                roots += Identifier(name)
            }
        }
      }

      for (root <- roots) {
        getPathRecursive(Vector[CableNode](), root)
      }

      Some(allNodes.result)
    } else {
      None
    }
  }

  /**
    * Convert the cable path into a piecewise linear approximation.
    *
    * @param cable List of nodes the cable must pass through.
    * @return List of points that approximate the curve
    */
  def createCablePoints(cable: Seq[CableNode]): Array[Vector3] = {
    require(cable.length >= 2, "Cable must have at least two nodes")

    val TimeEnd = (cable.length - 1).toDouble
    var position = cable.head.position
    var tangent = cable.head.tangentOut.normalize

    /** Evaluate the _whole_ cable at a point */
    def evaluate(t: Double): Vector3 = {
      if (t <= 0.0) return cable.head.position
      if (t >= TimeEnd) return cable.last.position

      val index = t.toInt
      val fract = t - index.toDouble
      val prev = cable(index)
      val next = cable(index + 1)
      val p0 = prev.position
      val m0 = prev.tangentOut
      val p1 = next.position
      val m1 = next.tangentIn
      Hermite.interpolate(p0, m0, p1, m1, fract)
    }

    val BinarySeachGranularity = 64
    val MinTimestep = 0.01
    val MaxTimestep = 0.2
    val MinDistance = 0.2
    val MaxDistance = 3.0
    val MinAngle = 15.0

    val minDistanceSq = MinDistance * MinDistance
    val maxDistanceSq = MaxDistance * MaxDistance
    val minAngleDot = math.cos(math.toRadians(MinAngle))
    val binarySearchStep = (MaxTimestep - MinTimestep) / BinarySeachGranularity.toDouble

    /** Returns whether this advance is of an acceptable distance and/or curvature */
    def isConsiderableAdvance(t: Double): Boolean = {
      val nextPos = evaluate(t)
      val delta = nextPos - position
      val sq = delta dot delta
      if (sq <= minDistanceSq) return false
      if (sq >= maxDistanceSq) return true

      val derivPos = evaluate(t + MinTimestep)
      val derivTangent = derivPos - nextPos
      val length = derivTangent.length
      if (derivTangent.length > 0.0001) {
        val dot = derivTangent.normalize dot tangent
        if (dot >= minAngleDot) return false
      }
      true
    }

    var time = 0.0

    val positions = ArrayBuffer[Vector3]()
    while (time < TimeEnd) {
      positions += position

      // Skip the cable one MaxTimestep at a time
      while (time < TimeEnd && !isConsiderableAdvance(time + MaxTimestep)) {
        time += MaxTimestep
      }

      // Find the optimal insert position
      val baseTime = time + MinTimestep
      val ix = BinarySearch.upperBound(0, BinarySeachGranularity, ix => {
        isConsiderableAdvance(baseTime + ix * binarySearchStep)
      })

      time = baseTime + ix * binarySearchStep
      val nextPos = evaluate(time)

      val deltaPos = nextPos - position
      val deltaLen = deltaPos.length
      if (deltaLen >= 0.0001) {
        tangent = deltaPos / deltaLen
      }
      position = nextPos
    }

    // The last two nodes can be the final node or one too close to it.
    // Remove them and add the final position manually.
    for (prune <- 0 until 2) {
      if ((positions.last - cable.last.position).length <= MinDistance)
        positions.remove(positions.length - 1)
    }
    positions += cable.last.position

    positions.toArray
  }

  def createCableMesh(cable: Seq[CableNode], radius: Double): CableMesh = createCableMesh(createCablePoints(cable), radius)
  def createCableMesh(points: Array[Vector3], radius: Double): CableMesh = {
    require(points.length >= 2, "Cable must have at least two endpoints")

    val mesh = new CableMesh()

    var normal: Vector3 = Vector3.Zero
    var tangent: Vector3 = Vector3.Zero
    var bitangent: Vector3 = Vector3.Zero

    var prevTangent: Vector3 = Vector3.Zero
    var prevBitangent: Vector3 = Vector3.Zero

    var length: Double = 0.0
    var prevPos = points(0)

    val numRings = points.length

    // Some slack in vertices for splitting into parts
    val numVerts = (numRings + 2) * VertsPerRing
    val vertexData = Memory.alloc(numVerts * CableSpec.sizeInBytes)

    val angleStepPerVert = (math.Pi * 2.0) / VertsPerRing

    val probeRet = new Array[Probe](4)
    val weightRet = new Array[Double](4)

    val parts = ArrayBuffer[CableMeshPart]()

    val partProbes = ArrayBuffer[Probe]()
    var partNumRings = 0

    var minX = Double.PositiveInfinity
    var minY = Double.PositiveInfinity
    var minZ = Double.PositiveInfinity
    var maxX = Double.NegativeInfinity
    var maxY = Double.NegativeInfinity
    var maxZ = Double.NegativeInfinity

    def flushCurrentPart(): Unit = {
      if (partNumRings == 0) return

      val part = new CableMeshPart(mesh)
      val finished = vertexData.duplicateEx
      finished.finish()
      part.numRings = partNumRings
      part.lightProbes = partProbes.toArray
      part.vertexBuffer = VertexBuffer.createStatic(CableSpec, finished)

      minX -= radius
      minY -= radius
      minZ -= radius
      maxX += radius
      maxY += radius
      maxZ += radius
      part.aabb = Aabb.fromMinMax(Vector3(minX, minY, minZ), Vector3(maxX, maxY, maxZ))
      minX = Double.PositiveInfinity
      minY = Double.PositiveInfinity
      minZ = Double.PositiveInfinity
      maxX = Double.NegativeInfinity
      maxY = Double.NegativeInfinity
      maxZ = Double.NegativeInfinity

      parts += part

      vertexData.position(0)
      partProbes.clear()
      partNumRings = 0
    }

    /** Append a ring based on a specified tangent frame */
    def appendRingVertices(pos: Vector3, up: Vector3, right: Vector3): Unit = {
      groundSystem.getProbesAndWeights(pos, probeRet, weightRet)

      var ix0 = partProbes.indexOf(probeRet(0))
      var ix1 = partProbes.indexOf(probeRet(1))
      var ix2 = partProbes.indexOf(probeRet(2))
      var ix3 = partProbes.indexOf(probeRet(3))

      val MaxProbes = LightProbeUniform.MaxProbes
      if (ix0 < 0 && partProbes.length < MaxProbes) { ix0 = partProbes.length; partProbes += probeRet(0) }
      if (ix1 < 0 && partProbes.length < MaxProbes) { ix1 = partProbes.length; partProbes += probeRet(1) }
      if (ix2 < 0 && partProbes.length < MaxProbes) { ix2 = partProbes.length; partProbes += probeRet(2) }
      if (ix3 < 0 && partProbes.length < MaxProbes) { ix3 = partProbes.length; partProbes += probeRet(3) }

      // Ran out of probe space: Emit previous and current rings again
      if (ix0 < 0 || ix1 < 0 || ix2 < 0 || ix3 < 0 || partNumRings >= PartMaxRings) {
        flushCurrentPart()
        appendRingVertices(prevPos, prevTangent, prevBitangent)
        appendRingVertices(pos, up, right)
        return
      }

      minX = math.min(pos.x, minX)
      minY = math.min(pos.y, minY)
      minZ = math.min(pos.z, minZ)
      maxX = math.max(pos.x, maxX)
      maxY = math.max(pos.y, maxY)
      maxZ = math.max(pos.z, maxZ)

      partNumRings += 1
      val wg0 = clamp(weightRet(0) * 255.0, 0, 255.0).toInt
      val wg1 = clamp(weightRet(1) * 255.0, 0, 255.0).toInt
      val wg2 = clamp(weightRet(2) * 255.0, 0, 255.0).toInt
      val wg3 = clamp(weightRet(3) * 255.0, 0, 255.0).toInt

      // Note: This is endian-independent since both vectors get
      // reversed between little and big endian systems and the shader uses
      // the vectors as unordered index-weight pairs.
      val probeIx = ix0 | ix1 << 8 | ix2 << 16 | ix3 << 24
      val probeWg = wg0 | wg1 << 8 | wg2 << 16 | wg3 << 24

      var ix = 0
      var angle = 0.0
      while (ix < VertsPerRing) {
        val s = math.sin(angle)
        val c = math.cos(angle)

        val nx = s*up.x + c*right.x
        val ny = s*up.y + c*right.y
        val nz = s*up.z + c*right.z

        vertexData.putFloat((pos.x + nx * radius).toFloat)
        vertexData.putFloat((pos.y + ny * radius).toFloat)
        vertexData.putFloat((pos.z + nz * radius).toFloat)
        vertexData.putFloat(length.toFloat)
        vertexData.putFloat(nx.toFloat)
        vertexData.putFloat(ny.toFloat)
        vertexData.putFloat(nz.toFloat)
        vertexData.putInt(probeIx)
        vertexData.putInt(probeWg)

        ix += 1
        angle += angleStepPerVert
      }
    }

    /** Append a ring based on the current tangent frame */
    def appendRing(pos: Vector3): Unit = {
      appendRingVertices(pos, tangent, bitangent)
      length += (pos - prevPos).length

      prevTangent = tangent
      prevBitangent = bitangent
      prevPos = pos
    }

    // Endpoint special case (start)
    {
      val p0 = points(0)
      val p1 = points(1)
      normal = (p1 - p0).normalize

      val tangentRef1 = if (math.abs(normal.y) < 0.8) Vector3(0.0, 1.0, 0.0) else Vector3(1.0, 0.0, 0.0)
      val tangentRef2 = (normal cross tangentRef1).normalize
      bitangent = (normal cross tangentRef2).normalize
      tangent = (normal cross bitangent).normalize

      appendRing(p0)
    }

    // Middle section
    var pointIx = 1
    while (pointIx < points.length - 1) {
      val p0 = points(pointIx - 1)
      val p1 = points(pointIx)
      val p2 = points(pointIx + 1)

      normal = (p1 - p0) + (p2 - p1)
      bitangent = (tangent cross normal).normalize
      tangent = (normal cross bitangent).normalize
      appendRing(p1)

      pointIx += 1
    }

    // Endpoint special case (end)
    {
      val p0 = points(points.length - 2)
      val p1 = points(points.length - 1)
      normal = (p1 - p0)
      bitangent = (tangent cross normal).normalize
      tangent = (normal cross bitangent).normalize
      appendRing(p1)
    }

    flushCurrentPart()
    Memory.free(vertexData)

    mesh.parts = parts.toArray
    mesh.numRings = points.length
    mesh.length = length

    mesh
  }

  override def createCable(entity: Entity, nodes: Seq[CableNode], radius: Double): Aabb = {
    val mesh = createCableMesh(nodes, radius)
    var min = Vector3(Double.MaxValue, Double.MaxValue, Double.MaxValue)
    var max = Vector3(Double.MinValue, Double.MinValue, Double.MinValue)

    for (part <- mesh.parts) {
      val partEntity = new Entity(true, CablePartName)
      partEntity.position = part.aabb.center
      parentingSystem.parentEntity(partEntity, entity)

      val localAabb = part.aabb.copy(center = Vector3.Zero)
      cullingSystem.addAabb(partEntity, localAabb, CullingSystem.MaskRender)

      min = Vector3.min(min, part.aabb.min)
      max = Vector3.max(max, part.aabb.max)

      for (probe <- part.lightProbes) {
        ambientSystem.addProbeDependency(partEntity, probe)
      }

      entityToCablePart(partEntity) = part
      partEntity.setFlag(Flag_CablePart)
    }

    Aabb.fromMinMax(min, max)
  }

  override def collectCableMeshes(visible: EntitySet) = {
    val partEntities = visible.flag(Flag_CablePart)
    partEntities.map(entityToCablePart)
  }

  override def entitiesDeleted(entities: EntitySet): Unit = {
    for (entity <- entities.flag(Flag_CablePart)) {
      val part = entityToCablePart(entity)
      entityToCablePart.remove(entity)
      part.unload()
    }
  }

  override def unload(): Unit = {
    assert(entityToCablePart.isEmpty)
    indexBuffer.free()
  }
}

