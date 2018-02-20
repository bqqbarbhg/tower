package render.opengl

import java.nio.ByteBuffer
import org.lwjgl.opengl.GL15._

import render._

object VertexBufferGl {
  var serialCounter = 0
  def nextSerial(): Int = {
    serialCounter += 1
    serialCounter
  }

  /** Create a vertex buffer with constant contents from `data` */
  def createStatic(spec: VertexSpec, data: ByteBuffer): VertexBufferGl = {
    val buf = new VertexBufferGl(spec)
    glBindBuffer(GL_ARRAY_BUFFER, buf.buffer)
    glBufferData(GL_ARRAY_BUFFER, data, GL_STATIC_DRAW)
    glBindBuffer(GL_ARRAY_BUFFER, 0)
    buf
  }
}

class VertexBufferGl(val spec: VertexSpec) {
  val serial: Int = VertexBufferGl.nextSerial()
  val buffer: Int = glGenBuffers()

  def free(): Unit = {
    glDeleteBuffers(buffer)
  }
}

