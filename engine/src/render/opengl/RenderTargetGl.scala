package render.opengl

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL30._

object RenderTargetGl {
  var Backbuffer: RenderTargetGl = null
}

class RenderTargetGl(val width: Int, val height: Int, val colorFormat: Array[String], val depthFormat: Option[String], val readableDepth: Boolean) {

  val fbo = if (colorFormat.nonEmpty || depthFormat.nonEmpty) {
    val fbo = glGenFramebuffers()
    glBindFramebuffer(GL_FRAMEBUFFER, fbo)
    fbo
  } else {
    0
  }

  val colorHandles = for ((format, index) <- colorFormat.zipWithIndex) yield {
    val tex = glGenTextures()
    glBindTexture(GL_TEXTURE_2D, tex)

    format match {
      case "RGBA" => glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0)
    }

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)

    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + index, GL_TEXTURE_2D, tex, 0)
    tex
  }

  val depthRenderbuffer = for (format <- depthFormat.filterNot(_ => readableDepth)) yield {
    val renderBuffer = glGenRenderbuffers()
    glBindRenderbuffer(GL_RENDERBUFFER, renderBuffer)

    format match {
      case "D24S" => glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8, width, height)
    }

    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_STENCIL, GL_RENDERBUFFER, renderBuffer)
    renderBuffer
  }

  val depthTexture = for (format <- depthFormat.filter(_ => readableDepth)) yield {
    val tex = glGenTextures()
    glBindTexture(GL_TEXTURE_2D, tex)

    format match {
      case "D24S" => glTexImage2D(GL_TEXTURE_2D, 0, GL_DEPTH24_STENCIL8, width, height, 0, GL_DEPTH_STENCIL, GL_UNSIGNED_INT_24_8, 0)
    }

    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_TEXTURE_2D, tex, 0)
    tex
  }

  def unload(): Unit = {
    depthRenderbuffer.map(o => glDeleteRenderbuffers(o))
    depthTexture.map(o => glDeleteTextures(o))
    glDeleteTextures(colorHandles)
    if (fbo != 0) glDeleteFramebuffers(fbo)
  }

}

