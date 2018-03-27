package render.opengl

import java.nio.ByteBuffer

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL21._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL31._
import org.lwjgl.opengl.GL32._
import org.lwjgl.opengl.GL33._
import org.lwjgl.system.MemoryUtil

import scala.collection.mutable.ArrayBuffer
import core._
import render._
import RendererGl._
import render.opengl.ProfilerGl.GpuTimeSpan

import scala.collection.mutable

object RendererGl {
  private var instance: RendererGl = null

  def initialize(): RendererGl = {
    assert(instance == null)
    instance = new RendererGl()
    instance
  }

  def shutdown(): Unit = {
    instance.unload()
    instance = null
  }

  def get: RendererGl = {
    assert(instance != null)
    instance
  }

  abstract class BlendMode(val enable: Boolean, val src: Int, val dst: Int)

  case object BlendNone extends BlendMode(false, GL_ONE, GL_ZERO)
  case object BlendAlpha extends BlendMode(true, GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
  case object BlendPremultipliedAlpha extends BlendMode(true, GL_ONE, GL_ONE_MINUS_SRC_ALPHA)
  case object BlendAddAlpha extends BlendMode(true, GL_SRC_ALPHA, GL_ONE)

  /**
    * A re-usable reference to an uniform block.
    * Note that it may have a limited lifetime depending on how it was obtained!
    */
  type UniformRef = AnyRef

  val QuadSpec = {
    import VertexSpec._
    VertexSpec(Vector(
      Attrib(2, DataFmt.F32, Identifier("Position")),
    ))
  }
}

class RendererGl {
  val vaoCache = new VaoCache()
  val samplerCache = new SamplerCache()
  val uniformAllocator = if (OptsGl.useUniformBlocks) new UniformAllocator(16*1024*1024) else null

  var activeShaderEnabled: Boolean = false
  var activeShader: ShaderProgramGl = null
  var activeUniforms: Array[UniformBlockRefGl] = Array[UniformBlockRefGl]()
  var activeUniformValues: Array[ByteBuffer] = Array[ByteBuffer]()
  var activeTextures: Array[Int] = Array[Int]()

  var frameIndex = 0

  var virtualUbosToFreeThisFrame = ArrayBuffer[ByteBuffer]()

  val frameTimespans = mutable.Queue[GpuTimeSpan]()
  var currentFrameSpan: Option[GpuTimeSpan] = None

  val frameTimeHistory = Array.fill(32)(0.0)
  var frameTimeHistoryIndex = 0

  val quadVertexBuffer = withStack {
    val data = alloca(QuadSpec.sizeInBytes * 12)
    data.putFloat(+1.0f); data.putFloat(-1.0f)
    data.putFloat(-1.0f); data.putFloat(+1.0f)
    data.putFloat(-1.0f); data.putFloat(-1.0f)
    data.putFloat(+1.0f); data.putFloat(-1.0f)
    data.putFloat(+1.0f); data.putFloat(+1.0f)
    data.putFloat(-1.0f); data.putFloat(+1.0f)
    data.finish()
    VertexBuffer.createStatic(QuadSpec, data).withLabel("Quad VB")
  }

  private var prevBlendMode: BlendMode = BlendNone

  private var writeSrgb: Boolean = false
  private var activeTarget: RenderTargetGl = null

  /** Returns the current render target */
  def currentRenderTarget: RenderTargetGl = {
    require(activeTarget != null)
    activeTarget
  }

  /** Set the current target to render to */
  def setRenderTarget(target: RenderTargetGl): Unit = {
    require(target != null)
    activeTarget = target
    glBindFramebuffer(GL_FRAMEBUFFER, target.fbo)
    glViewport(0, 0, target.width, target.height)

    if (target.numSamples > 1)
      glEnable(GL_MULTISAMPLE)
    else
      glDisable(GL_MULTISAMPLE)
  }

  /** Resizes and initializes the backbuffer */
  def resizeBackbuffer(width: Int, height: Int): Unit = {
    if (RenderTargetGl.Backbuffer == null || RenderTargetGl.Backbuffer.width != width || RenderTarget.Backbuffer.height != height) {
      if (RenderTargetGl.Backbuffer != null) RenderTargetGl.Backbuffer.unload()
      RenderTargetGl.Backbuffer = new RenderTargetGl(width, height, Array[String](), None, false, 1)
    }
  }

  /** Needs to be called every frame */
  def beginFrame(): Unit = {
    frameIndex += 1

    vaoCache.advanceFrame()
    samplerCache.advanceFrame()
    if (uniformAllocator != null)
      uniformAllocator.advanceFrame()
    ProfilerGl.advanceFrame()

    java.util.Arrays.fill(activeUniforms.asInstanceOf[Array[AnyRef]], null)

    for (ubo <- virtualUbosToFreeThisFrame)
      MemoryUtil.memFree(ubo)
    virtualUbosToFreeThisFrame.clear()

    while (frameTimespans.nonEmpty && frameTimespans.front.ready) {
      val ts = frameTimespans.dequeue()
      val index = frameTimeHistoryIndex
      frameTimeHistoryIndex = (frameTimeHistoryIndex + 1) % frameTimeHistory.length
      val durationMs = (ts.durationNs.toDouble / 1000).toDouble / 1000.0
      frameTimeHistory(index) = durationMs
    }

    if (OptsGl.useProfiling) {
      assert(currentFrameSpan.isEmpty)
      currentFrameSpan = Some(ProfilerGl.startSpan())
    }
  }

  def endFrame(): Unit = {
    for (span <- currentFrameSpan) {
      span.stop()
      frameTimespans.enqueue(span)
    }
    currentFrameSpan = None
  }

  def averageFrameTime: Double = frameTimeHistory.sum / frameTimeHistory.length.toDouble

  def setInvalidShader(): Unit = {
    activeShader = null
    activeShaderEnabled = false
  }

  def setShader(shader: ShaderProgramGl): Unit = {
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
    val ref = pushUniformRef(uniform, writeData)
    bindUniform(uniform, ref)
  }

  /**
    * Push uniform data for future use. Retruns a reference that can be used with
    * `bindUniform()`
    *
    * WARNING: The pushed uniform reference is valid for only one frame!
    *
    * @param uniform Type of uniform block to set
    * @param writeData Callback that should write the data to its argument
    * @return A reference to the pushed uniform block. NOTE: Valid for only this frame!
    */
  def pushUniformRef(uniform: UniformBlock, writeData: ByteBuffer => Unit): UniformRef = {
    if (OptsGl.useUniformBlocks) {
      // Uniform block implementation: Write to GPU memory
      uniformAllocator.push(uniform.sizeInBytes, writeData)
    } else {
      // Old-school uniform implemntation, write virtual block to CPU memory
      // and upload the uniforms later.
      val buf = MemoryUtil.memAlloc(uniform.sizeInBytes)
      virtualUbosToFreeThisFrame += buf
      writeData(buf.sliceEx)
      buf
    }
  }

  /**
    * Bind a previously written uniform block.
    *
    * @param uniform Type of uniform block to set
    * @param ref Reference to the uniform block to set
    */
  def bindUniform(uniform: UniformBlock, ref: UniformRef): Unit = {
    if (OptsGl.useUniformBlocks) {

      if (activeUniforms.length <= uniform.serial) {
        val old = activeUniforms
        activeUniforms = new Array[UniformBlockRefGl](UniformBlock.maxSerial)
        java.lang.System.arraycopy(old, 0, activeUniforms, 0, old.length)
      }

      activeUniforms(uniform.serial) = ref.asInstanceOf[UniformBlockRefGl]
    } else {

      if (activeUniformValues.length <= uniform.serial) {
        val old = activeUniformValues
        activeUniformValues = new Array[ByteBuffer](UniformBlock.maxSerial)
        java.lang.System.arraycopy(old, 0, activeUniformValues, 0, old.length)
      }

      activeUniformValues(uniform.serial) = ref.asInstanceOf[ByteBuffer]
    }
  }

  def setTexture(sampler: SamplerBlock.USampler2D, texture: TextureHandleGl): Unit = {
    val samplerObj = samplerCache.getSampler(sampler.sampler)
    glActiveTexture(GL_TEXTURE0 + sampler.index)
    glBindTexture(GL_TEXTURE_2D, texture.texture)
    glBindSampler(sampler.index, samplerObj)
  }

  def setTexture(sampler: SamplerBlock.USampler2DArray, texture: TextureHandleGl): Unit = {
    val samplerObj = samplerCache.getSampler(sampler.sampler)
    glActiveTexture(GL_TEXTURE0 + sampler.index)
    glBindTexture(GL_TEXTURE_2D_ARRAY, texture.texture)
    glBindSampler(sampler.index, samplerObj)
  }

  def setTextureTargetColor(sampler: SamplerBlock.USampler2DMS, target: RenderTarget, index: Int): Unit = {
    glActiveTexture(GL_TEXTURE0 + sampler.index)
    glBindTexture(GL_TEXTURE_2D_MULTISAMPLE, target.colorHandles(index))
    glBindSampler(sampler.index, 0)
  }

  def setTextureTargetColor(sampler: SamplerBlock.USampler2D, target: RenderTarget, index: Int): Unit = {
    val samplerObj = samplerCache.getSampler(sampler.sampler)
    glActiveTexture(GL_TEXTURE0 + sampler.index)
    glBindTexture(GL_TEXTURE_2D, target.colorHandles(index))
    glBindSampler(sampler.index, samplerObj)
  }

  def setTextureTargetDepth(sampler: SamplerBlock.USampler2D, target: RenderTarget): Unit = {
    val samplerObj = samplerCache.getSampler(sampler.sampler)
    glActiveTexture(GL_TEXTURE0 + sampler.index)
    glBindTexture(GL_TEXTURE_2D, target.depthTexture.get)
    glBindSampler(sampler.index, samplerObj)
  }

  def applyState(): Unit = {
    if (!activeShaderEnabled) {
      glUseProgram(activeShader.program)
      activeShaderEnabled = true

      for (sampler <- activeShader.samplers) {
        glUniform1i(sampler.shaderIndex, sampler.index)
      }
    }

    if (OptsGl.useUniformBlocks) {

      for (uniform <- activeShader.uniforms) {
        val ref = activeUniforms(uniform.serial)
        glBindBufferRange(GL_UNIFORM_BUFFER, uniform.shaderIndex, ref.buffer, ref.offset, ref.size)
      }

    } else {

      for (uniform <- activeShader.uniformValues) {
        val block = activeUniformValues(uniform.blockSerial)
        val loc = uniform.shaderIndex
        val off = uniform.blockOffset
        val num = uniform.arraySize
        uniform.glType match {
          case GL_FLOAT_VEC4 => glUniform4fv(loc, block.slicedOffset(off, num * 16).asFloatBuffer)
          case GL_FLOAT_MAT4x3 => glUniformMatrix4x3fv(loc, true, block.slicedOffset(off, num * 48).asFloatBuffer)
          case GL_FLOAT_MAT4 => glUniformMatrix4fv(loc, true, block.slicedOffset(off, num * 64).asFloatBuffer)
        }
      }

    }
  }

  def setDepthMode(write: Boolean, test: Boolean): Unit = {
    glDepthMask(write)
    if (test) glEnable (GL_DEPTH_TEST)
    else      glDisable(GL_DEPTH_TEST)
  }

  def setWriteSrgb(enabled: Boolean): Unit = {
    writeSrgb = enabled
    if (enabled) glEnable (GL_FRAMEBUFFER_SRGB)
    else         glDisable(GL_FRAMEBUFFER_SRGB)
  }

  def setBlend(mode: BlendMode): Unit = {
    if (mode == prevBlendMode) return

    if (mode.enable != prevBlendMode.enable) {
      if (mode.enable) glEnable(GL_BLEND)
      else             glDisable(GL_BLEND)
    }

    if (mode.enable)
      glBlendFunc(mode.src, mode.dst)

    prevBlendMode = mode
  }

  def setCull(enable: Boolean): Unit = {
    if (enable) {
      glEnable(GL_CULL_FACE)
      glFrontFace(GL_CCW)
      glCullFace(GL_BACK)
    } else {
      glDisable(GL_CULL_FACE)
    }
  }

  def blitRenderTargetColor(dst: RenderTargetGl, src: RenderTargetGl): Unit = {
    glBindFramebuffer(GL_READ_FRAMEBUFFER, src.fbo)

    if (dst.fbo == 0) {
      glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0)
      glDrawBuffer(GL_BACK)
    } else {
      glBindFramebuffer(GL_DRAW_FRAMEBUFFER, dst.fbo)
      glDrawBuffer(GL_COLOR_ATTACHMENT0)
    }

    val x1 = src.width
    val y1 = src.height
    glBlitFramebuffer(0, 0, x1, y1, 0, 0, x1, y1, GL_COLOR_BUFFER_BIT, GL_NEAREST)

    glBindFramebuffer(GL_READ_FRAMEBUFFER, 0)
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0)
  }

  def clear(color: Option[Color], depth: Option[Double]): Unit = {
    var flags = 0x0
    for (c <- color) {
      flags |= GL_COLOR_BUFFER_BIT
      if (writeSrgb) {
        val r = c.r.toFloat
        val g = c.g.toFloat
        val b = c.b.toFloat
        val a = c.a.toFloat
        glClearColor(r, g, b, a)
      } else {
        val r = Color.linearToSrgb(c.r).toFloat
        val g = Color.linearToSrgb(c.g).toFloat
        val b = Color.linearToSrgb(c.b).toFloat
        val a = c.a.toFloat
        glClearColor(r, g, b, a)
      }
    }
    for (d <- depth) {
      flags |= GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT
      glClearDepth(d.toFloat)
    }

    if (flags != 0)
      glClear(flags)
  }

  def drawElements(num: Int, ib: IndexBufferGl, vb0: VertexBufferGl, vb1: VertexBufferGl = null, baseVertex: Int = 0): Unit = {
    if (activeShader != null) {
      applyState()
      vaoCache.bindVertexBuffers(activeShader, vb0, vb1, ib)
      if (baseVertex != 0) {
        glDrawElementsBaseVertex(GL_TRIANGLES, num, GL_UNSIGNED_SHORT, 0, baseVertex)
      } else {
        glDrawElements(GL_TRIANGLES, num, GL_UNSIGNED_SHORT, 0)
      }
      glBindVertexArray(0)
    }
  }

  def drawElementsInstanced(numInstances: Int, num: Int, ib: IndexBufferGl, vb0: VertexBufferGl, vb1: VertexBufferGl = null, baseVertex: Int = 0): Unit = {
    if (activeShader != null) {
      applyState()
      vaoCache.bindVertexBuffers(activeShader, vb0, vb1, ib)
      if (baseVertex != 0) {
        glDrawElementsInstancedBaseVertex(GL_TRIANGLES, num, GL_UNSIGNED_SHORT, 0, numInstances, baseVertex)
      } else {
        glDrawElementsInstanced(GL_TRIANGLES, num, GL_UNSIGNED_SHORT, 0, numInstances)
      }
      glBindVertexArray(0)
    }
  }

  def drawTriangles(num: Int, vb0: VertexBufferGl, vb1: VertexBufferGl = null, baseVertex: Int = 0): Unit = {
    if (activeShader != null) {
      applyState()
      vaoCache.bindVertexBuffers(activeShader, vb0, vb1, null)
      glDrawArrays(GL_TRIANGLES, baseVertex, num)
      glBindVertexArray(0)
    }
  }

  def drawLines(num: Int, vb0: VertexBufferGl, vb1: VertexBufferGl = null, baseVertex: Int = 0): Unit = {
    if (activeShader != null) {
      applyState()
      vaoCache.bindVertexBuffers(activeShader, vb0, vb1, null)
      glDrawArrays(GL_LINES, baseVertex, num)
      glBindVertexArray(0)
    }
  }

  def drawQuad(): Unit = {
    drawTriangles(6, quadVertexBuffer)
  }

  def unload(): Unit = {
    if (uniformAllocator != null)
      uniformAllocator.unload()
    samplerCache.unload()
    vaoCache.unload()
    quadVertexBuffer.free()
    RenderTarget.Backbuffer.unload()
    RenderTarget.Backbuffer = null
  }
}

