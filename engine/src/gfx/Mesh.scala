package gfx

import java.nio.ByteBuffer

import org.lwjgl.system.MemoryUtil
import core._
import render._
import util.BufferUtils._
import io.content.Package

object Mesh {

  def load(name: Identifier): Option[Mesh] = {
    Package.get.get(name).map(file => {
      val mesh = new Mesh()

      val buffer = MemoryUtil.memAlloc(file.sizeInBytes.toInt)
      val stream = file.read()
      buffer.readFrom(stream)
      buffer.finish()
      mesh.load(buffer)
      stream.close()
      MemoryUtil.memFree(buffer)

      if (mesh.parts.length > 1) {
        for ((part, index) <- mesh.parts.zipWithIndex) {
          part.vertexBuffer.setLabel(s"$name VB (part $index)")
          part.indexBuffer.setLabel(s"$name IB (part $index)")
        }
      } else {
        for (part <- mesh.parts) {
          part.vertexBuffer.setLabel(s"$name VB")
          part.indexBuffer.setLabel(s"$name IB")
        }
      }

      mesh
    })
  }

}

class Mesh {

  var parts: Array[MeshPart] = null
  var material: Material = null

  def load(buffer: ByteBuffer): Unit = {

    val MaxVersion = 1
    buffer.verifyMagic("s2ms")
    val version = buffer.getVersion(MaxVersion)

    val numParts = buffer.getInt()

    parts = Array.tabulate(numParts)(ix => {
      val part = new MeshPart()
      part.load(buffer)
      part
    })

    buffer.verifyMagic("E.ms")
  }
}

