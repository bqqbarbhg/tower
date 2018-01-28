package authoring.resource

import util.math._

case class FrameRot(time: Double, rot: Quaternion)
// case class FramePos(time: Double, pos: Vector3)
// case class FrameSize(time: Double, size: Vector3)

class Timeline {
  val boneName: String = ""
  // val rot = new Array[FrameRot]
  // val pos = new Array[FramePos]
  // val size = new Array[FrameSize]
}

class Animation(name: String) extends authoring.Resource(name) {


}
