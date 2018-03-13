package game.system

import core._
import util.BinarySearch

import scala.collection.mutable.ArrayBuffer

object CableRenderSystem {

  /**
    * Represents a single node in the Hermite-interpolation based cable curve.
    *
    * @param position Position of the node
    * @param tangent Tangent direction and magnitude
    */
  case class CableNode(position: Vector3, tangent: Vector3)

  class CableMesh {
  }


  /**
    * Convert the cable path into a piecewise linear approximation.
    *
    * @param cable List of nodes the cable must pass through.
    * @return List of points that approximate the curve
    */
  def createCablePoints(cable: Seq[CableNode]): ArrayBuffer[Vector3] = {
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

    positions
  }

}
