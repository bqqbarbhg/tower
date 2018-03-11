package render.opengl

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.KHRDebug._

object DebugGl {

  val BUFFER = GL_BUFFER
  val PROGRAM = GL_PROGRAM
  val TEXTURE = GL_TEXTURE
  val SAMPLER = GL_SAMPLER
  val FRAMEBUFFER = GL_FRAMEBUFFER
  val RENDERBUFFER = GL_RENDERBUFFER

  def setLabel(identifier: Int, name: Int, label: String): Unit = {
    if (OptsGl.useDebug) {
      glObjectLabel(identifier, name, label)
    }
  }

}

