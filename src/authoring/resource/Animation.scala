package authoring.resource

import util.math._

import scala.collection.mutable.ArrayBuffer

case class FrameRot(time: Double, rot: Quaternion)
// case class FramePos(time: Double, pos: Vector3)
// case class FrameSize(time: Double, size: Vector3)

case class Timeline(val boneName: String, val rot: Array[FrameRot])

class Animation(name: String) extends authoring.Resource(name) {

  var timelines = new ArrayBuffer[Timeline]

}
