package game.system

import core._
import game.lighting.LightProbe
import game.options.Options
import game.shader._
import org.lwjgl.system.MemoryUtil
import render._
import util.BinarySearch

import scala.collection.mutable.ArrayBuffer

class CableMeshPart {
  var lightProbes: Array[LightProbe] = null
  var vertexBuffer: VertexBuffer = null
  var numRings: Int = 0

  def draw(): Unit = {
    val renderer = Renderer.get
    var baseRing = 0

    renderer.pushUniform(LightProbeUniform, u => LightProbeUniform.write(u, lightProbes))

    val maxRing = numRings - 1
    while (baseRing < maxRing) {
      val toDraw = math.min(maxRing - baseRing, CableRenderSystem.MaxRingsPerDraw)
      val quadsToDraw = CableRenderSystem.VertsPerRing * toDraw
      val baseVertex = baseRing * CableRenderSystem.VertsPerRing
      renderer.drawElements(quadsToDraw * 6, CableRenderSystem.indexBuffer, vertexBuffer, baseVertex = baseVertex)
      baseRing += toDraw
    }
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

/**
  * Represents a single node in the Hermite-interpolation based cable curve.
  *
  * @param position Position of the node
  * @param tangent Tangent direction and magnitude
  */
case class CableNode(position: Vector3, tangent: Vector3)

class CableRenderSystem {

  val Quality = Options.current.graphics.quality.modelQuality

  val VertsPerRing = Quality match {
    case 0|1 => 4
    case 2 => 6
    case 3 => 8
  }
  val MaxRingsPerDraw = 2048

  val indexBuffer = {
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
      Attrib(4, UI8, Identifier("ProbeIndex")),
      Attrib(4, UN8, Identifier("ProbeWeight")),
    ))
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

    var prevTangent: Vector3 = Vector3.Zero
    var prevBitangent: Vector3 = Vector3.Zero

    var length: Double = 0.0
    var prevPos = points(0)

    val numRings = points.length
    val numVerts = numRings * VertsPerRing
    val vertexData = MemoryUtil.memAlloc(numVerts * CableSpec.sizeInBytes)

    val angleStepPerVert = (math.Pi * 2.0) / VertsPerRing

    val probeRet = new Array[LightProbe](4)
    val weightRet = new Array[Double](4)

    val parts = ArrayBuffer[CableMeshPart]()

    val partProbes = ArrayBuffer[LightProbe]()
    var partNumRings = 0

    def flushCurrentPart(): Unit = {
      val part = new CableMeshPart()
      val finished = vertexData.duplicateEx
      finished.finish()
      part.numRings = partNumRings
      part.lightProbes = partProbes.toArray
      part.vertexBuffer = VertexBuffer.createStatic(CableSpec, finished)
      parts += part

      partNumRings = 0
    }

    /** Append a ring based on a specified tangent frame */
    def appendRingVertices(pos: Vector3, up: Vector3, right: Vector3): Unit = {
      GroundSystem.getProbesAndWeights(pos, probeRet, weightRet)

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
      if (ix0 < 0 || ix1 < 0 || ix2 < 0 || ix3 < 0) {
        flushCurrentPart()
        appendRingVertices(prevPos, prevTangent, prevBitangent)
        appendRingVertices(pos, up, right)
        return
      }

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
    MemoryUtil.memFree(vertexData)

    val mesh = new CableMesh()
    mesh.parts = parts.toArray
    mesh.numRings = points.length
    mesh.length = length

    mesh
  }

  def unload(): Unit = {
    indexBuffer.free()
  }

}
