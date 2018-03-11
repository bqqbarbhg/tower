package input.device

import org.lwjgl.glfw.GLFW._
import org.lwjgl.glfw.GLFWKeyCallbackI
import Keyboard._

object Keyboard {

 val NameToSpecial = Map(
  "Up" -> GLFW_KEY_UP,
  "Down" -> GLFW_KEY_DOWN,
  "Left" ->  GLFW_KEY_LEFT,
  "Right" -> GLFW_KEY_RIGHT,
  "Space" -> GLFW_KEY_SPACE,
 )

  val SpecialToName = NameToSpecial.map(_.swap)

  val NumKeys = GLFW_KEY_LAST + 1
}

class Keyboard extends InputDevice {

  var state = Array.fill(NumKeys * 3)(false)
  val prevOffset = 0
  val curOffset = NumKeys
  val writeOffset = NumKeys * 2

  val keyCallback = new GLFWKeyCallbackI {
    override def invoke(window: Long, key: Int, scancode: Int, action: Int, mods: Int): Unit = {
      onKey(key, scancode, action, mods)
    }
  }

  def onKey(key: Int, scancode: Int, action: Int, mods: Int): Unit = this.synchronized {
    if (key < 0 || key >= NumKeys) return

    val ix = writeOffset + key
    if (action == GLFW_PRESS) {
      state(ix) = true
    } else if (action == GLFW_RELEASE) {
      state(ix) = false
    }
  }

  override def deviceType: InputDevice.DeviceType = InputDevice.Keyboard

  override def isButtonDown(index: Int): Boolean = state(curOffset + index)
  override def wasButtonDown(index: Int): Boolean = state(prevOffset + index)

  override def update(): Unit = this.synchronized {
    java.lang.System.arraycopy(state, curOffset, state, prevOffset, NumKeys)
    java.lang.System.arraycopy(state, writeOffset, state, curOffset, NumKeys)
  }

  override def nameToButtonIndex(name: String): Option[Int] = {
    if (name.length == 1 && name(0) >= 'A' && name(0) <= 'Z') {
      Some(name(0) - 'A' + GLFW_KEY_A)
    } else if (name.length == 1 && name(0) >= '0' && name(0) <= '9') {
      Some(name(0) - '0' + GLFW_KEY_0)
    } else {
      NameToSpecial.get(name)
    }
  }

  override def buttonIndexToName(index: Int): Option[String] = {
    if (index >= GLFW_KEY_A && index <= GLFW_KEY_Z) {
      Some(('A' + (index - GLFW_KEY_A)).toString)
    } else if (index >= GLFW_KEY_0 && index <= GLFW_KEY_9) {
      Some(('0' + (index - GLFW_KEY_0)).toString)
    } else {
      SpecialToName.get(index)
    }
  }

}
