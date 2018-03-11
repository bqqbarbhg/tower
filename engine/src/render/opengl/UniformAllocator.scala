package render.opengl

import java.nio.ByteBuffer

import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL31._
import org.lwjgl.opengl.GL44._
import core._
import org.lwjgl.system.MemoryUtil

class UniformAllocator(val bufferSize: Int) {

  private val uniformAlignment = math.max(glGetInteger(GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT), 64)

  private def uniformAlign(size: Int): Int = {
    size + (uniformAlignment - size % uniformAlignment) % uniformAlignment
  }

  private val alignedBufferSize = uniformAlign(bufferSize)
  private var offset = 0
  private var lastWrapFrame = 0
  private var frameIndex = 0

  private var mapMode = OptsGl.uniformMap

  private var buffer = 0
  private var persistentMap: ByteBuffer = null

  // Init
  {
    buffer = glGenBuffers()
    glBindBuffer(GL_UNIFORM_BUFFER, buffer)
    DebugGl.setLabel(DebugGl.BUFFER, buffer, "UniformAllocator Buffer")

    if (mapMode.persistent) {
      if (GL.getCapabilities.GL_ARB_buffer_storage) {
        var flag = GL_MAP_WRITE_BIT | GL_MAP_PERSISTENT_BIT
        if (mapMode.coherent) flag |= GL_MAP_COHERENT_BIT
        var mapFlag = flag
        if (!mapMode.coherent) mapFlag |= GL_MAP_FLUSH_EXPLICIT_BIT
        glBufferStorage(GL_UNIFORM_BUFFER, alignedBufferSize, flag)
        persistentMap = glMapBufferRange(GL_UNIFORM_BUFFER, 0, alignedBufferSize, mapFlag)
      } else {
        mapMode = OptsGl.uniformMapFallback
      }
    }

    if (persistentMap == null)
      glBufferData(GL_UNIFORM_BUFFER, alignedBufferSize, GL_DYNAMIC_DRAW)

    glBindBuffer(GL_UNIFORM_BUFFER, 0)
  }

  def advanceFrame(): Unit = {
    frameIndex += 1
  }

  def push(size: Int, writeData: ByteBuffer => Unit): UniformBlockRefGl = withStack {
    val alignedSize = uniformAlign(size)
    if (offset + alignedSize > alignedBufferSize) {
      assert(frameIndex > lastWrapFrame + 4)
      lastWrapFrame = frameIndex
      offset = 0
    }

    val loc = offset
    offset += alignedSize

    glBindBuffer(GL_UNIFORM_BUFFER, buffer)

    mapMode match {
      case MapMode.Map =>
        val buf = glMapBufferRange(GL_UNIFORM_BUFFER, loc, size, GL_MAP_WRITE_BIT|GL_MAP_UNSYNCHRONIZED_BIT)
        writeData(buf)
        glUnmapBuffer(GL_UNIFORM_BUFFER)

      case MapMode.SubData =>
        val buf = alloca(size)
        writeData(buf)
        buf.position(0)
        glBufferSubData(GL_UNIFORM_BUFFER, loc, buf)

      case MapMode.Persistent|MapMode.PersistentCoherent =>
        val buf = persistentMap.sliced(loc, size)
        writeData(buf)
        if (!mapMode.coherent)
          glFlushMappedBufferRange(GL_UNIFORM_BUFFER, loc, size)

      case MapMode.PersistentCopy|MapMode.PersistentCopyCoherent =>
        val buf = alloca(size)
        writeData(buf)
        buf.position(0)
        val copy = persistentMap.slicedOffset(loc, size)
        MemoryUtil.memCopy(buf, copy)
        if (!mapMode.coherent)
          glFlushMappedBufferRange(GL_UNIFORM_BUFFER, loc, size)
    }

    glBindBuffer(GL_UNIFORM_BUFFER, 0)

    UniformBlockRefGl(buffer, loc, size)
  }

  def unload(): Unit = {
    if (persistentMap != null) {
      glBindBuffer(GL_UNIFORM_BUFFER, buffer)
      glUnmapBuffer(GL_UNIFORM_BUFFER)
      glBindBuffer(GL_UNIFORM_BUFFER, 0)
      persistentMap = null
    }
    glDeleteBuffers(buffer)
    buffer = 0
  }
}

