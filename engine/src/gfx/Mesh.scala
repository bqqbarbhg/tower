package gfx

import java.nio.ByteBuffer

import org.lwjgl.system.MemoryUtil
import core._
import io.ContentFile
import render._
import util.BufferUtils._
import io.content.Package
import task._

object Mesh {

  def load(name: Identifier): Option[Mesh] = Some(deferredLoad(name).get)

  def deferredLoad(name: Identifier): Task[Mesh] = {
    ContentFile.load(name, buffer => {
      val mesh = new Mesh()
      mesh.load(buffer)

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
  var numSkinnedParts: Int = 0

  def load(buffer: ByteBuffer): Unit = {

    val MaxVersion = 1
    buffer.verifyMagic("s2ms")
    val version = buffer.getVersion(MaxVersion)

    val numParts = buffer.getInt()

    parts = Array.tabulate(numParts)(ix => {
      val part = new MeshPart(this)
      part.load(buffer)

      if (part.numBones > 0) {
        val index = numSkinnedParts
        numSkinnedParts += 1
        part.skinnedPartIndex = index
      }

      part
    })

    buffer.verifyMagic("E.ms")
  }

  def unload(): Unit = {
    for (part <- parts) {
      part.unload()
    }
  }

  def draw(): Unit = {
    for (part <- parts) {
      part.draw()
    }
  }
}

