package input.device

import input.InputSet.Button

object InputDevice {
  sealed abstract class DeviceType(val name: String)
  case object Keyboard extends DeviceType("Keyboard")
}

abstract class InputDevice {

  def deviceType: InputDevice.DeviceType
  def update(): Unit

  def nameToButtonIndex(name: String): Option[Int]
  def buttonIndexToName(index: Int): Option[String]

  def isButtonDown(index: Int): Boolean
  def wasButtonDown(index: Int): Boolean

}

