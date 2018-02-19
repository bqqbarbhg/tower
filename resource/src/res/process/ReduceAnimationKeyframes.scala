package res.process

import collection.mutable.ArrayBuffer

import core._
import res.intermediate._
import res.intermediate.Animation._

object ReduceAnimationKeyframes {

  /**
    * Removes intervals of keyframes that could be replaced by interpolating between their endpoints.
    *
    * @param numKeyframes Total number of original keyframes
    * @param canReplaceFn Function that returns true if the interval (begin, end) can be reduced into
    *                     those two endpoints. Guarantee: `begin + 1 < end`
    * @return Array keyframe indices to _keep_ from the originals
    *
    * Complexity: O(n log n)
    */
  private def reduceKeyframes(numKeyframes: Int, canReplaceFn: (Int, Int) => Boolean): ArrayBuffer[Int] = {
    // Explicitly handle degenerate cases
    numKeyframes match {
      case 0 => return ArrayBuffer[Int]()
      case 1 => return ArrayBuffer[Int](0)
      case 2 => return ArrayBuffer[Int](0, 1)
      case _ => // Do the actual reduction
    }

    // Wrap the user callback handing degenerate cases gracefully
    def canRangeBeReplaced(begin: Int, end: Int): Boolean = {
      if (begin + 1 >= end) return true
      canReplaceFn(begin, end)
    }

    // Always keep the first keyframe
    val keyframesToKeep = new ArrayBuffer[Int]()
    keyframesToKeep += 0

    // Scan through all the keyframes replacing all possible intervals
    var currentIndex = 0
    while (currentIndex + 1 < numKeyframes) {

      // Binary search for the first keyframe that would cause too much error
      val firstUnacceptableIndex = {
        var first = currentIndex
        var count = numKeyframes - currentIndex
        while (count > 0) {
          val step = count / 2
          val index = first + step

          if (canRangeBeReplaced(currentIndex, index)) {
            first = index + 1
            count -= step + 1
          } else {
            count = step
          }
        }
        first
      }

      // The interval ]currentIndex, lastAcceptableIndex[ can be discarded while maintaining quality bounds.
      val lastAcceptableIndex = firstUnacceptableIndex - 1

      // `lastAcceptableIndex` is guaranteed to be greater than `currentIndex` -> forward progress is guaranteed.
      assert(currentIndex + 1 < numKeyframes)    // Due to the `while` condition
      assert(lastAcceptableIndex > currentIndex) // Due to above and `canRangeBeReplaced` explicit degenerate case

      keyframesToKeep += lastAcceptableIndex
      currentIndex = lastAcceptableIndex
    }

    keyframesToKeep
  }

  /**
    * Removes rotation keyframes that could be replaced by linear interpolation (within an error margin).
    */
  def reduceRotationKeyframes(timeline: Timeline, maximumError: Double): Timeline = {

    def canRangeBeReplaced(begin: Int, end: Int): Boolean = {
      val a: FrameQuat = timeline.rot(begin)
      val b: FrameQuat = timeline.rot(end)

      for (index <- (begin + 1) to (end - 1)) {
        val mid = timeline.rot(index)
        val t = (mid.time - a.time) / (b.time - a.time)
        val quat = Quaternion.lerp(a.value, b.value, t).normalize
        if ((mid.value - quat).length > maximumError) return false
      }

      true
    }

    val keepFrames = reduceKeyframes(timeline.rot.length, canRangeBeReplaced)
    val newKeyframes = keepFrames.map(timeline.rot)
    timeline.copy(rot = newKeyframes.toArray)
  }

  /**
    * Removes position keyframes that could be replaced by linear interpolation (within a relative error margin).
    */
  def reducePositionKeyframes(timeline: Timeline, maximumError: Double): Timeline = {

    def canRangeBeReplaced(begin: Int, end: Int): Boolean = {
      val a: FrameVec3 = timeline.pos(begin)
      val b: FrameVec3 = timeline.pos(end)

      for (index <- (begin + 1) to (end - 1)) {
        val mid = timeline.pos(index)
        val t = (mid.time - a.time) / (b.time - a.time)
        val vec = Vector3.lerp(a.value, b.value, t)
        if ((mid.value - vec).length > maximumError) return false
      }

      true
    }

    val keepFrames = reduceKeyframes(timeline.pos.length, canRangeBeReplaced)
    val newKeyframes = keepFrames.map(timeline.pos)
    timeline.copy(pos = newKeyframes.toArray)
  }

  /**
    * Removes size keyframes that could be replaced by linear interpolation (within a relative error margin).
    */
  def reduceSizeKeyframes(timeline: Timeline, maximumError: Double): Timeline = {

    def canRangeBeReplaced(begin: Int, end: Int): Boolean = {
      val a: FrameVec3 = timeline.size(begin)
      val b: FrameVec3 = timeline.size(end)

      for (index <- (begin + 1) to (end - 1)) {
        val mid = timeline.size(index)
        val t = (mid.time - a.time) / (b.time - a.time)
        val vec = Vector3.lerp(a.value, b.value, t)
        if ((mid.value - vec).length > maximumError) return false
      }

      true
    }

    val keepFrames = reduceKeyframes(timeline.size.length, canRangeBeReplaced)
    val newKeyframes = keepFrames.map(timeline.size)
    timeline.copy(size = newKeyframes.toArray)
  }

}
