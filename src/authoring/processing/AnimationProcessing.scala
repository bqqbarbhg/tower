package tower.authoring.processing

import tower.authoring.resource._
import tower.math._

import scala.collection.mutable.ArrayBuffer

object AnimationProcessing {

  /**
    * "Canonicalizes" the rotation quaternions in a way that the distance between
    * successive keyframes' rotations is minimal. The problem is that the same
    * rotation can be described by two quaternions (x, y, z, w) and (-x, -y, -z, -w).
    * This just linearly scans the animation through and selects the better option.
    */
  def flipQuaternions(timeline: Timeline): Timeline = {
    val newRot = ArrayBuffer[FrameQuat](timeline.rot.head)
    for (rot <- timeline.rot.tail) {
      val ref = newRot.last.value
      val inv = -rot.value
      val quat = if ((ref - rot.value).length > (ref - inv).length) {
        inv
      } else {
        rot.value
      }
      newRot += rot.copy(value = quat)
    }
    timeline.copy(rot = newRot.toArray)
  }

  /**
    * "Canonicalizes" all quaternions in the animation, see the comment on the other
    * overload for more info.
    */
  def flipQuaternions(animation: AnimationResource): Unit = {
    animation.timelines = animation.timelines.map(flipQuaternions)
  }

}
