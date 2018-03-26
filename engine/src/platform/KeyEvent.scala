package platform

import org.lwjgl.glfw.GLFW._

object KeyEvent {
  val Enter = GLFW_KEY_ENTER
  val Escape = GLFW_KEY_ESCAPE
  val Backspace = GLFW_KEY_BACKSPACE
  val Delete = GLFW_KEY_DELETE
  val Home = GLFW_KEY_HOME
  val End = GLFW_KEY_END

  val Left = GLFW_KEY_LEFT
  val Right = GLFW_KEY_RIGHT
  val Up = GLFW_KEY_UP
  val Down = GLFW_KEY_DOWN
}

case class KeyEvent(key: Int, action: Int, mods: Int) {
  def shift: Boolean   = (mods & 0x01) != 0
  def control: Boolean = (mods & 0x02) != 0
  def alt: Boolean     = (mods & 0x04) != 0

  def down: Boolean = action == GLFW_PRESS
  def up: Boolean = action == GLFW_RELEASE
  def repeat: Boolean = action == GLFW_REPEAT
}

