package game.options

import io.{SimpleSerializable, SimpleVisitor}
import platform.KeyEvent

class BindsOptions extends SimpleSerializable {

  var cameraUp: String = "W"
  var cameraDown: String = "S"
  var cameraLeft: String = "A"
  var cameraRight: String = "D"
  var cameraBoost: String = "LeftShift"
  var delete: String = "Delete"
  var rotate: String = "Rotate"

  var bar1: String = "Num1"
  var bar2: String = "Num2"
  var bar3: String = "Num3"
  var bar4: String = "Num4"
  var bar5: String = "Num5"
  var bar6: String = "Num6"
  var bar7: String = "Num7"
  var bar8: String = "Num8"
  var bar9: String = "Num9"

  var invertScroll: Boolean = false

  override def visit(v: SimpleVisitor): Unit = {
    cameraUp = v.field("cameraUp", cameraUp)
    cameraDown = v.field("cameraDown", cameraDown)
    cameraLeft = v.field("cameraLeft", cameraLeft)
    cameraRight = v.field("cameraRight", cameraRight)
    cameraBoost = v.field("cameraBoost", cameraBoost)
    invertScroll = v.field("invertScroll", invertScroll)
    delete = v.field("delete", delete)
    rotate = v.field("rotate", rotate)
  }

}

