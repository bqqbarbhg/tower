package render.opengl

import org.lwjgl.opengl.GL11._

object GraphicsDeviceGl {
  private lazy val instance = new GraphicsDeviceGl

  def get: GraphicsDeviceGl = instance
}

class GraphicsDeviceGl {

  /** Get the vendor of the GL implementation */
  def version: String = glGetString(GL_VERSION)

  /** Get the vendor of the GL implementation */
  def vendor: String = glGetString(GL_VENDOR)

  /** Get the driver of the GL implementation */
  def driver: String = glGetString(GL_RENDERER)

}
