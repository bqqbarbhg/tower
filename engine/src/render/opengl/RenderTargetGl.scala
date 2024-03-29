package render.opengl

import java.nio.ByteBuffer

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL21._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL32._
import RenderTargetGl._
import core.Vector2

object RenderTargetGl {
  var Backbuffer: RenderTargetGl = null

  def create(width: Int, height: Int, colorFormat: Iterable[String], depthFormat: Option[String], readableDepth: Boolean, numSamples: Int = 1): RenderTargetGl = {
    new RenderTargetGl(width, height, colorFormat.toArray, depthFormat, readableDepth, numSamples)
  }

  lazy val MaxSamples = glGetInteger(GL_MAX_SAMPLES)

}

class RenderTargetGl(val width: Int, val height: Int, val colorFormat: Array[String], val depthFormat: Option[String], val readableDepth: Boolean, val numSamples: Int) {

  val fbo = if (colorFormat.nonEmpty || depthFormat.nonEmpty) {
    val fbo = glGenFramebuffers()
    glBindFramebuffer(GL_FRAMEBUFFER, fbo)
    fbo
  } else {
    0
  }

  val debugRes = debug.ResourceHandle("gl.renderTarget")

  val clampedSamples = if (numSamples > 1) math.min(numSamples, MaxSamples) else 0

  val colorHandles = for ((format, index) <- colorFormat.zipWithIndex) yield {
    val tex = glGenTextures()
    val binding = if (numSamples > 1) GL_TEXTURE_2D_MULTISAMPLE else GL_TEXTURE_2D

    glBindTexture(binding, tex)

    if (numSamples > 1) {
      format match {
        case "RGBA" => glTexImage2DMultisample(binding, clampedSamples, GL_RGBA8, width, height, true)
        case "SRGA" => glTexImage2DMultisample(binding, clampedSamples, GL_SRGB8, width, height, true)
        case "Rf10" => glTexImage2DMultisample(binding, clampedSamples, GL_R11F_G11F_B10F, width, height, true)
        case "Rf16" => glTexImage2DMultisample(binding, clampedSamples, GL_RGB16F, width, height, true)
        case "Rf32" => glTexImage2DMultisample(binding, clampedSamples, GL_RGB32F, width, height, true)
      }
    } else {
      format match {
        case "RGBA" => glTexImage2D(binding, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0)
        case "SRGA" => glTexImage2D(binding, 0, GL_SRGB, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0)
        case "Rf10" => glTexImage2D(binding, 0, GL_R11F_G11F_B10F, width, height, 0, GL_RGB, GL_FLOAT, 0)
        case "Rf16" => glTexImage2D(binding, 0, GL_RGB16F, width, height, 0, GL_RGB, GL_HALF_FLOAT, 0)
        case "Rf32" => glTexImage2D(binding, 0, GL_RGB32F, width, height, 0, GL_RGB, GL_FLOAT, 0)
      }

      glTexParameteri(binding, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
      glTexParameteri(binding, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    }

    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0 + index, binding, tex, 0)
    tex
  }

  val depthRenderbuffer = for (format <- depthFormat.filterNot(_ => readableDepth)) yield {
    val renderBuffer = glGenRenderbuffers()
    glBindRenderbuffer(GL_RENDERBUFFER, renderBuffer)

    if (numSamples > 1) {
      format match {
        case "D24S" => glRenderbufferStorageMultisample(GL_RENDERBUFFER, clampedSamples, GL_DEPTH24_STENCIL8, width, height)
      }
    } else {
      format match {
        case "D24S" => glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH24_STENCIL8, width, height)
      }
    }

    glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, GL_RENDERBUFFER, renderBuffer)
    renderBuffer
  }

  val depthTexture = for (format <- depthFormat.filter(_ => readableDepth)) yield {
    val tex = glGenTextures()
    val binding = if (numSamples > 1) GL_TEXTURE_2D_MULTISAMPLE else GL_TEXTURE_2D
    glBindTexture(binding, tex)

    if (numSamples > 1) {
      format match {
        case "D24S" => glTexImage2DMultisample(binding, clampedSamples, GL_DEPTH24_STENCIL8, width, height, true)
      }
    } else {
      format match {
        case "D24S" => glTexImage2D(binding, 0, GL_DEPTH24_STENCIL8, width, height, 0, GL_DEPTH_STENCIL, GL_UNSIGNED_INT_24_8, 0)
      }

      glTexParameteri(binding, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
      glTexParameteri(binding, GL_TEXTURE_MAG_FILTER, GL_LINEAR)
    }

    glFramebufferTexture2D(GL_FRAMEBUFFER, GL_DEPTH_STENCIL_ATTACHMENT, binding, tex, 0)
    tex
  }

  def readColorPixels(index: Int, buffer: ByteBuffer, format: String): Unit = {
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0)
    glBindFramebuffer(GL_READ_FRAMEBUFFER, fbo)
    glReadBuffer(GL_COLOR_ATTACHMENT0 + index)

    format match {
      case "Rf32" => glReadPixels(0, 0, width, height, GL_RGB, GL_FLOAT, buffer)
    }
  }

  def unload(): Unit = {
    depthRenderbuffer.map(o => glDeleteRenderbuffers(o))
    depthTexture.map(o => glDeleteTextures(o))
    if (colorHandles.nonEmpty)
      glDeleteTextures(colorHandles)
    if (fbo != 0) glDeleteFramebuffers(fbo)
    debugRes.free()
  }

  def setLabel(label: String): Unit = {
    if (OptsGl.useDebug) {
      DebugGl.setLabel(DebugGl.FRAMEBUFFER, fbo, label)

      for ((handle, index) <- colorHandles.zipWithIndex) {
        val texLabel = s"$label: color $index"
        DebugGl.setLabel(DebugGl.TEXTURE, handle, texLabel)
      }

      for (handle <- depthTexture) {
        val texLabel = s"$label: depth"
        DebugGl.setLabel(DebugGl.TEXTURE, handle, texLabel)
      }

      for (handle <- depthRenderbuffer) {
        val texLabel = s"$label: depth (non-readable)"
        DebugGl.setLabel(DebugGl.RENDERBUFFER, handle, texLabel)
      }
    }
  }

  def withLabel(label: String): RenderTargetGl = {
    setLabel(label)
    this
  }

  lazy val sampleLocations: Seq[Vector2] = {
    if (numSamples <= 1) {
      Array(Vector2(0.5, 0.5))
    } else {
      glBindFramebuffer(GL_FRAMEBUFFER, fbo)
      Array.tabulate(numSamples)(sample => {
        val arr = Array(0.5f, 0.5f)
        glGetMultisamplefv(GL_SAMPLE_POSITION, sample, arr)
        Vector2(arr(0), arr(1))
      })
    }
  }

  def aspectRatio: Double = width.toDouble / height.toDouble

}

