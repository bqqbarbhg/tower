package render.opengl

import java.nio.ByteBuffer
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL31._

class UniformAllocator(val bufferSize: Int) {

  private val uniformAlignment = glGetInteger(GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT)
  private val buffer = glGenBuffers()

  private def uniformAlign(size: Int): Int = {
    size + (uniformAlignment - size % uniformAlignment) % uniformAlignment
  }

  private val alignedBufferSize = uniformAlign(bufferSize)
  private var offset = 0
  private var lastWrapFrame = 0
  private var frameIndex = 0

  // Init
  {
    glBindBuffer(GL_UNIFORM_BUFFER, buffer)
    glBufferData(GL_UNIFORM_BUFFER, alignedBufferSize, GL_DYNAMIC_DRAW)
  }

  def advanceFrame(): Unit = {
    frameIndex += 1
  }

  def push(size: Int, writeData: ByteBuffer => Unit): UniformBlockRefGl = {
    val alignedSize = uniformAlign(size)
    if (offset + alignedSize > alignedBufferSize) {
      assert(frameIndex > lastWrapFrame + 4)
      lastWrapFrame = frameIndex
      offset = 0
    }

    glBindBuffer(GL_UNIFORM_BUFFER, buffer)
    val mapping = glMapBufferRange(GL_UNIFORM_BUFFER, offset, alignedSize, GL_MAP_WRITE_BIT | GL_MAP_UNSYNCHRONIZED_BIT)
    writeData(mapping)
    glUnmapBuffer(GL_UNIFORM_BUFFER)
    glBindBuffer(GL_UNIFORM_BUFFER, 0)

    UniformBlockRefGl(buffer, offset, size)
  }

  def unload(): Unit = {
    glDeleteBuffers(buffer)
  }
}

