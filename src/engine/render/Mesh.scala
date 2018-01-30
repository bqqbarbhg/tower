package tower.engine.render

import java.io.InputStream
import java.nio.ByteBuffer

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL30._

import tower.util.Serialization.ByteBufferExtension
import tower.Identifier

import Mesh._

object Mesh {
  val VertexSizeBytes = 3*4 + 2*4 + 4 + 4 + 4
  val IndexSizeBytes = 2
}

class Mesh {

  var name: Identifier = Identifier.Empty
  var vertexBuffer: Int = 0
  var indexBuffer: Int = 0
  var vertexArray: Int = 0

  var numVertices: Int = 0
  var numIndices: Int = 0

  def load(buffer: ByteBuffer): Unit = {

    // @Serialize(s2ms)

    // File header
    buffer.verifyMagic("s2ms")
    val MaxVersion = 1
    val version = buffer.getVersion(MaxVersion)

    // Mesh header
    this.name = buffer.getIdentifier()
    this.numVertices = buffer.getInt()
    this.numIndices = buffer.getInt()

    // Create the vertex and index buffers
    val bufs = new Array[Int](2)
    glGenBuffers(bufs)
    vertexBuffer = bufs(0)
    indexBuffer = bufs(1)
    glBindBuffer(GL_ARRAY_BUFFER, vertexBuffer)
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexBuffer)

    // Upload the vertex data
    {
      val vertexDataSize = numVertices * VertexSizeBytes
      val vertexBytes = buffer.duplicateEx()
      vertexBytes.limit(vertexBytes.position + vertexDataSize)
      buffer.position(vertexBytes.limit)
      glBufferData(GL_ARRAY_BUFFER, vertexBytes, GL_STATIC_DRAW)
    }

    // Upload the index data
    {
      val indexDataSize = numIndices * IndexSizeBytes
      val indexBytes = buffer.duplicateEx()
      indexBytes.limit(indexBytes.position + indexDataSize)
      buffer.position(indexBytes.limit)
      glBufferData(GL_ELEMENT_ARRAY_BUFFER, indexBytes, GL_STATIC_DRAW)
    }

    // Seutp the VAO
    vertexArray = glGenVertexArrays()
    glBindVertexArray(vertexArray)
    glEnableVertexAttribArray(0)
    glBindBuffer(GL_ARRAY_BUFFER, vertexBuffer)
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, indexBuffer)
    glVertexAttribPointer(0, 4, GL_FLOAT, false, Mesh.VertexSizeBytes, 0)

  }

  def unload(): Unit = {
    val bufs = Array[Int](vertexBuffer, indexBuffer)
    glDeleteBuffers(bufs)
    glDeleteVertexArrays(vertexArray)
    vertexBuffer = 0
    indexBuffer = 0
    vertexArray = 0
  }

}
