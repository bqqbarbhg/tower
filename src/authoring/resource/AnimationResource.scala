package tower.authoring.resource

import tower.math._

import scala.collection.mutable.ArrayBuffer

case class FrameQuat(time: Double, value: Quaternion)
case class FrameVec3(time: Double, value: Vector3)

case class Timeline(val boneName: String, val rot: Array[FrameQuat], val pos: Array[FrameVec3], val size: Array[FrameVec3])

class AnimationResource(name: String, val duration: Double) extends tower.authoring.Resource(name) {

  var ticksPerSecond: Double = 1.0
  var timelines = new ArrayBuffer[Timeline]

}
