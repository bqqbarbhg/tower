package render.opengl
import java.nio.ByteBuffer

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL31._

import core._
import render._

object RendererGl {
  private var instance: RendererGl = null

  def initialize(): RendererGl = {
    assert(instance == null)
    instance = new RendererGl()
    instance
  }

  def get: RendererGl = {
    assert(instance != null)
    instance
  }
}

class RendererGl {

  val vaoCache = new VaoCache()
  val uniformAllocator = new UniformAllocator(1024*1024)

  var activeShaderEnabled: Boolean = false
  var activeShader: ShaderGl = null
  var activeUniforms: Array[UniformBlockRefGl] = Array[UniformBlockRefGl]()

  /** Needs to be called every frame */
  def advanceFrame(): Unit = {
    vaoCache.advanceFrame()
    uniformAllocator.advanceFrame()

    val error = glGetError()
    if (error != 0) {
      println(s"GL error: $error")
    }

    java.util.Arrays.fill(activeUniforms.asInstanceOf[Array[AnyRef]], null)
  }

  def setShader(shader: ShaderGl): Unit = {
    activeShader = shader
    activeShaderEnabled = false
  }

  /**
    * Push uniform data to shaders.
    *
    * @param uniform Type of uniform block to set
    * @param writeData Callback that should write the data to its argument
    */
  def pushUniform(uniform: UniformBlock, writeData: ByteBuffer => Unit): Unit = {
    if (activeUniforms.length <= uniform.serial) {
      val old = activeUniforms
      activeUniforms = new Array[UniformBlockRefGl](UniformBlock.maxSerial)
      java.lang.System.arraycopy(old, 0, activeUniforms, 0, old.length)
    }

    val ref = uniformAllocator.push(uniform.sizeInBytes, writeData)
    activeUniforms(uniform.serial) = ref
  }

  def applyState(): Unit = {
    if (!activeShaderEnabled) {
      glUseProgram(activeShader.program)
      activeShaderEnabled = true
    }

    for (uniform <- activeShader.uniforms) {
      val ref = activeUniforms(uniform.serial)
      glBindBufferRange(GL_UNIFORM_BUFFER, uniform.shaderIndex, ref.buffer, ref.offset, ref.size)
    }
  }

  def clear(color: Option[Color], depth: Option[Double]): Unit = {
    var flags = 0x0
    for (c <- color) {
      flags |= GL_COLOR_BUFFER_BIT
      val r = Color.linearToSrgb(c.r).toFloat
      val g = Color.linearToSrgb(c.g).toFloat
      val b = Color.linearToSrgb(c.b).toFloat
      val a = c.a.toFloat
      glClearColor(r, g, b, a)
    }
    for (d <- depth) {
      flags |= GL_DEPTH_BUFFER_BIT
      glClearDepth(d.toFloat)
    }

    if (flags != 0)
      glClear(flags)
  }

  def drawElements(num: Int, ib: IndexBufferGl, vb0: VertexBufferGl, vb1: VertexBufferGl = null): Unit = {
    applyState()
    vaoCache.bindVertexBuffers(activeShader, vb0, vb1)
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ib.buffer)
    glDrawElements(GL_TRIANGLES, num, GL_UNSIGNED_SHORT, 0)
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0)
    glBindVertexArray(0)
  }

  def unload(): Unit = {
    uniformAllocator.unload()
    vaoCache.unload()
  }
}

