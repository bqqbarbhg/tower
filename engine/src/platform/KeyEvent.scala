package platform

import org.lwjgl.glfw.GLFW._

import scala.collection.mutable

object KeyEvent {
  val Enter = GLFW_KEY_ENTER
  val Escape = GLFW_KEY_ESCAPE
  val Backspace = GLFW_KEY_BACKSPACE
  val Delete = GLFW_KEY_DELETE
  val Home = GLFW_KEY_HOME
  val End = GLFW_KEY_END

  val LeftShift = GLFW_KEY_LEFT_SHIFT

  val Left = GLFW_KEY_LEFT
  val Right = GLFW_KEY_RIGHT
  val Up = GLFW_KEY_UP
  val Down = GLFW_KEY_DOWN

  val KeyToName = {
    val map = mutable.HashMap[Int, String](
      GLFW_KEY_SPACE -> "Space",
      GLFW_KEY_APOSTROPHE -> "Apostrophe",
      GLFW_KEY_COMMA -> "Comma",
      GLFW_KEY_MINUS -> "Minus",
      GLFW_KEY_PERIOD -> "Period",
      GLFW_KEY_SLASH -> "Slash",
      GLFW_KEY_SEMICOLON -> "Semicolon",
      GLFW_KEY_EQUAL -> "Equal",
      GLFW_KEY_LEFT_BRACKET -> "LeftBracket",
      GLFW_KEY_BACKSLASH -> "Backslash",
      GLFW_KEY_RIGHT_BRACKET -> "RightBracket",
      GLFW_KEY_GRAVE_ACCENT -> "Grave",
      GLFW_KEY_WORLD_1 -> "World1",
      GLFW_KEY_WORLD_2 -> "World2",
      GLFW_KEY_ESCAPE -> "Escape",
      GLFW_KEY_ENTER -> "Enter",
      GLFW_KEY_TAB -> "Tab",
      GLFW_KEY_BACKSPACE -> "Backspace",
      GLFW_KEY_INSERT -> "Insert",
      GLFW_KEY_DELETE -> "Delete",
      GLFW_KEY_RIGHT -> "Right",
      GLFW_KEY_LEFT -> "Left",
      GLFW_KEY_DOWN -> "Down",
      GLFW_KEY_UP -> "Up",
      GLFW_KEY_PAGE_UP -> "PageUp",
      GLFW_KEY_PAGE_DOWN -> "PageDown",
      GLFW_KEY_HOME -> "Home",
      GLFW_KEY_END -> "End",
      GLFW_KEY_CAPS_LOCK -> "CapsLock",
      GLFW_KEY_SCROLL_LOCK -> "ScrollLock",
      GLFW_KEY_NUM_LOCK -> "NumLock",
      GLFW_KEY_PRINT_SCREEN -> "PrintScreen",
      GLFW_KEY_PAUSE -> "Pause",
      GLFW_KEY_KP_DECIMAL -> "KpDecimal",
      GLFW_KEY_KP_DIVIDE -> "KpDivide",
      GLFW_KEY_KP_MULTIPLY -> "KpMultiply",
      GLFW_KEY_KP_SUBTRACT -> "KpSubtract",
      GLFW_KEY_KP_ADD -> "KpAdd",
      GLFW_KEY_KP_ENTER -> "KpEnter",
      GLFW_KEY_KP_EQUAL -> "KpEqual",
      GLFW_KEY_LEFT_SHIFT -> "LeftShift",
      GLFW_KEY_LEFT_CONTROL -> "LeftControl",
      GLFW_KEY_LEFT_ALT -> "LeftAlt",
      GLFW_KEY_LEFT_SUPER -> "LeftSuper",
      GLFW_KEY_RIGHT_SHIFT -> "RightShift",
      GLFW_KEY_RIGHT_CONTROL -> "RightControl",
      GLFW_KEY_RIGHT_ALT -> "RightAlt",
      GLFW_KEY_RIGHT_SUPER -> "RightSuper",
      GLFW_KEY_MENU -> "Menu",
    )

    for (k <- 0 to 9) {
      map(GLFW_KEY_0 + k) = s"Num$k"
    }

    for (k <- 'A' to 'Z') {
      map(GLFW_KEY_A + k.toInt - 'A'.toInt) = s"$k"
    }

    for (k <- 1 to 25) {
      map(GLFW_KEY_F1 + k) = s"F$k"
    }

    for (k <- 0 to 9) {
      map(GLFW_KEY_KP_0 + k) = s"KpNum$k"
    }

    map.toMap
  }

  val NameToKey = KeyToName.map(_.swap)
}

case class KeyEvent(key: Int, action: Int, mods: Int) {
  def shift: Boolean   = (mods & 0x01) != 0
  def control: Boolean = (mods & 0x02) != 0
  def alt: Boolean     = (mods & 0x04) != 0

  def down: Boolean = action == GLFW_PRESS
  def up: Boolean = action == GLFW_RELEASE
  def repeat: Boolean = action == GLFW_REPEAT
}

