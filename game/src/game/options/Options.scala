package game.options

import io.SimpleSerialization.SMap
import io.{SimpleSerializable, SimpleVisitor}

object Options {
  var current = new Options()

  val WindowModes = Vector(
    "Window",
    "Fullscreen",
    "Borderless",
  )
}

class Options extends SimpleSerializable {

  var windowMode: String = "Window"
  var monitor: Int = 0
  var windowSizeX: Int = 1280
  var windowSizeY: Int = 720
  var language: String = "en"

  var graphics = new GraphicsOptions()
  var audio = new AudioOptions()

  def copy: Options = {
    val map = SMap.read(this)
    val copy = new Options()
    map.write(copy)
    copy
  }

  override def visit(v: SimpleVisitor): Unit = {
    windowMode = v.field("windowMode", windowMode)
    monitor = v.field("monitor", monitor)
    windowSizeX = v.field("windowSizeX", windowSizeX)
    windowSizeY = v.field("windowSizeY", windowSizeY)
    language = v.field("language", language)
    graphics = v.field("graphics", graphics)
    audio = v.field("audio", audio)
  }
}

