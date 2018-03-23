package render.opengl

import debug.ResourceHandle
import java.nio.ByteBuffer
import org.lwjgl.opengl.GL15._

import render._

object IndexBufferGl {
  var serialCounter = 0
  def nextSerial(): Int = {
    serialCounter += 1
    serialCounter
  }

  /** Create a index buffer with constant contents from `data` */
  def createStatic(data: ByteBuffer): IndexBufferGl = {
    val buf = new IndexBufferGl(data.remaining / 2)
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buf.buffer)
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, data, GL_STATIC_DRAW)
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0)
    buf
  }
}

class IndexBufferGl(val numIndices: Int) {
  val serial: Int = IndexBufferGl.nextSerial()
  val buffer: Int = glGenBuffers()
  val debugRes = ResourceHandle("gl.indexBuffer")

  def free(): Unit = {
    glDeleteBuffers(buffer)
    debugRes.free()
  }

  def setLabel(label: String): Unit = DebugGl.setLabel(DebugGl.BUFFER, buffer, label)
  def withLabel(label: String): IndexBufferGl = {
    setLabel(label)
    this
  }
}
