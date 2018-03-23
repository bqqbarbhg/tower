package game.options

import game.options.GraphicsOptions.OpenGlOptions
import io.{SimpleSerializable, SimpleVisitor}
import render.opengl.MapMode

object GraphicsOptions {

  class OpenGlOptions extends SimpleSerializable {

    var uniformMapMode: String = "Persistent"
    var vertexMapMode: String = "Persistent"
    var useUniformBuffers: Boolean = true

    override def visit(v: SimpleVisitor): Unit = {
      uniformMapMode = v.field("uniformMapMode", uniformMapMode)
      vertexMapMode = v.field("vertexMapMode", vertexMapMode)
      useUniformBuffers = v.field("useUniformBuffers", useUniformBuffers)
    }

  }

  object OpenGlOptions {
    val MapModes = Vector("SubData", "Map", "Persistent", "PersistentCopy", "PersistentCoherent", "PersistentCopyCoherent")

    def mapModeToEnum(name: String, fallback: MapMode): MapMode = {
      name match {
        case "SubData" => MapMode.SubData
        case "Map" => MapMode.Map
        case "Persistent" => MapMode.Persistent
        case "PersistentCopy" => MapMode.PersistentCopy
        case "PersistentCoherent" => MapMode.PersistentCoherent
        case "PersistentCopyCoherent" => MapMode.PersistentCopyCoherent
        case other =>
          println(s"Map mode '$name' does not exist!")
          fallback
      }
    }
  }

}

class GraphicsOptions extends SimpleSerializable {
  var openGl = new OpenGlOptions()

  override def visit(v: SimpleVisitor): Unit = {
    openGl = v.field("openGl", openGl)
  }
}

