package authoring.processing

import authoring.resource._
import util.math._

import scala.collection.mutable.ArrayBuffer

object AnimationOptimization {

  /**
    * Removes rotation keyframes that could be replaced by linear interpolation (within an error margin).
    *
    * Complexity: O(n log n)
    */
  def reduceRotationKeyframes(timeline: Timeline, maximumError: Double): Timeline = {

    // Explicitly handle degenerate cases
    if (timeline.rot.length <= 2) return timeline

    /** Returns whether a linear interpolation between the begin (inclusive) and end (inclusive) indices represents
      * the whole interval within the error margin. */
    def canRangeBeReplaced(begin: Int, end: Int): Boolean = {

      // Explicitly handle degenerate cases
      if (begin + 1 >= end) return true

      val a: FrameRot = timeline.rot(begin)
      val b: FrameRot = timeline.rot(end)

      // Linearly scan through all the midpoints and check that the interpolated value matches the actual values
      for (index <- (begin + 1) to (end - 1)) {
        val mid = timeline.rot(index)
        val t = (mid.time - a.time) / (b.time - a.time)
        val quat = Quaternion.lerp(a.rot, b.rot, t).normalize
        if ((mid.rot - quat).length > maximumError) return false
      }

      true
    }

    val newKeyframes = new ArrayBuffer[FrameRot]()
    val numKeyframes = timeline.rot.length

    // Always insert the first keyframe
    newKeyframes += timeline.rot(0)

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

      newKeyframes += timeline.rot(lastAcceptableIndex)
      currentIndex = lastAcceptableIndex
    }

    timeline.copy(rot = newKeyframes.toArray)
  }

}
