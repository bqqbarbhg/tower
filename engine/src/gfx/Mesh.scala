package gfx

import java.nio.ByteBuffer

import core._
import render._
import util.BufferUtils._

class Mesh {

  var parts: Array[MeshPart] = null

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

