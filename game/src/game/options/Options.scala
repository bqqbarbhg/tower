package game.options

import io.SimpleSerialization.SMap
import io.{SimpleSerializable, SimpleVisitor}

object Options {
  var current = new Options()
}

class Options extends SimpleSerializable {
  var graphics = new GraphicsOptions()

  def copy: Options = {
    val map = SMap.read(this)
    val copy = new Options()
    map.write(copy)
    copy
  }

  override def visit(v: SimpleVisitor): Unit = {
    graphics = v.field("graphics", graphics)
  }
}

