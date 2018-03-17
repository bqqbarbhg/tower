package game.system

import core._
import org.lwjgl.system.MemoryUtil
import render._
import util.BinarySearch

import scala.collection.mutable.ArrayBuffer

object CableRenderSystem {

  val VertsPerRing = 6
  val MaxRingsPerDraw = 2048

  lazy val indexBuffer = {
    val data = MemoryUtil.memAlloc(VertsPerRing * MaxRingsPerDraw * 6 * 2)

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
    MemoryUtil.memFree(data)
    indexBuffer.withLabel("CableRenderSsytem.indexBuffer")
  }

  val CableSpec = {
    import render.VertexSpec.Attrib
    import render.VertexSpec.DataFmt._
    VertexSpec(Vector(
      Attrib(3, F32, Identifier("Position")),
      Attrib(1, F32, Identifier("Distance")),
      Attrib(3, F32, Identifier("Normal")),
    ))
  }

  /**
    * Represents a single node in the Hermite-interpolation based cable curve.
    *
    * @param position Position of the node
    * @param tangent Tangent direction and magnitude
    */
  case class CableNode(position: Vector3, tangent: Vector3)

  class CableMesh {
    var vertexBuffer: VertexBuffer = null
    var numRings: Int = 0
    var length: Double = 0.0

    def draw(): Unit = {
      var baseRing = 0
      val renderer = Renderer.get

      val maxRing = numRings - 1
      while (baseRing < maxRing) {
        val toDraw = math.min(maxRing - baseRing, MaxRingsPerDraw)
        val quadsToDraw = VertsPerRing * toDraw
        val baseVertex = baseRing * VertsPerRing
        renderer.drawElements(quadsToDraw * 6, indexBuffer, vertexBuffer, baseVertex = baseVertex)
        baseRing += toDraw
      }

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
    var tangent = cable.head.tangent.normalize

    /** Evaluate the _whole_ cable at a point */
    def evaluate(t: Double): Vector3 = {
      if (t <= 0.0) return cable.head.position
      if (t >= TimeEnd) return cable.last.position

      val index = t.toInt
      val fract = t - index.toDouble
      val prev = cable(index)
      val next = cable(index + 1)
      val p0 = prev.position
      val m0 = prev.tangent
      val p1 = next.position
      val m1 = next.tangent
      Hermite.interpolate(p0, m0, p1, m1, fract)
    }

    val BinarySeachGranularity = 16
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

      tangent = (nextPos - position).normalize
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

    var normal: Vector3 = Vector3.Zero
    var tangent: Vector3 = Vector3.Zero
    var bitangent: Vector3 = Vector3.Zero

    var length: Double = 0.0
    var prevPos = points(0)

    val numRings = points.length
    val numVerts = numRings * VertsPerRing
    val vertexData = MemoryUtil.memAlloc(numVerts * CableSpec.sizeInBytes)

    val angleStepPerVert = (math.Pi * 2.0) / VertsPerRing

    /** Append a ring based on the current tangent frame */
    def appendRing(pos: Vector3): Unit = {
      var ix = 0
      var angle = 0.0

      length += (pos - prevPos).length

      while (ix < VertsPerRing) {
        val s = math.sin(angle)
        val c = math.cos(angle)

        val nx = s*tangent.x + c*bitangent.x
        val ny = s*tangent.y + c*bitangent.y
        val nz = s*tangent.z + c*bitangent.z

        vertexData.putFloat((pos.x + nx * radius).toFloat)
        vertexData.putFloat((pos.y + ny * radius).toFloat)
        vertexData.putFloat((pos.z + nz * radius).toFloat)
        vertexData.putFloat(length.toFloat)
        vertexData.putFloat(nx.toFloat)
        vertexData.putFloat(ny.toFloat)
        vertexData.putFloat(nz.toFloat)

        ix += 1
        angle += angleStepPerVert
      }

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

    vertexData.finish()

    val mesh = new CableMesh()
    mesh.vertexBuffer = VertexBuffer.createStatic(CableSpec, vertexData)
    mesh.numRings = points.length
    mesh.length = length
    MemoryUtil.memFree(vertexData)

    mesh
  }

}
