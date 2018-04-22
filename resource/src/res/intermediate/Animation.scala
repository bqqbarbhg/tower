package res.intermediate

import scala.collection.mutable.ArrayBuffer

import core._
import Animation._

object Animation {
  case class FrameQuat(time: Double, value: Quaternion)
  case class FrameVec3(time: Double, value: Vector3)

  case class Timeline(val boneName: String, val isParent: Boolean, val rot: Array[FrameQuat], val pos: Array[FrameVec3], val size: Array[FrameVec3])
}

class Animation(val name: String, var duration: Double, var ticksPerSecond: Double) extends Resource {
  var timelines = new ArrayBuffer[Timeline]()

  override def unload(): Unit = {
    timelines = new ArrayBuffer[Timeline]()
  }
}
