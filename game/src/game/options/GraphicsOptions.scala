package game.options

import game.options.GraphicsOptions.OpenGlOptions
import io.{SimpleSerializable, SimpleVisitor}

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
  }

}

class GraphicsOptions extends SimpleSerializable {
  var openGl = new OpenGlOptions()

  override def visit(v: SimpleVisitor): Unit = {
    openGl = v.field("openGl", openGl)
  }
}

