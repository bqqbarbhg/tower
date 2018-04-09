package game.options

import io.{SimpleSerializable, SimpleVisitor}
import platform.KeyEvent

class BindsOptions extends SimpleSerializable {

  var cameraUp: String = "W"
  var cameraDown: String = "S"
  var cameraLeft: String = "A"
  var cameraRight: String = "D"
  var cameraBoost: String = "LeftShift"

  override def visit(v: SimpleVisitor): Unit = {
    cameraUp = v.field("cameraUp", cameraUp)
    cameraDown = v.field("cameraDown", cameraDown)
    cameraLeft = v.field("cameraLeft", cameraLeft)
    cameraRight = v.field("cameraRight", cameraRight)
    cameraBoost = v.field("cameraBoost", cameraBoost)
  }

}

