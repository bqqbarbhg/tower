package gfx

import java.nio.ByteBuffer

import core._
import render._
import util.BufferUtils._

class Texture {

  var width: Int = 0
  var height: Int = 0
  var numLevels: Int = 0
  var format: String = ""
  var texture: TextureHandle = null

  def load(buffer: ByteBuffer): Unit = {
    // @Deserialize(s2tx)

    val MaxVersion = 1
    buffer.verifyMagic("s2tx")
    val version = buffer.getVersion(MaxVersion)

    width = buffer.getInt()
    height = buffer.getInt()
    numLevels = buffer.getInt()
    format = buffer.getMagic()

    val levels = Seq.fill(numLevels) {
      val size = buffer.getInt()
      buffer.getBuffer(size)
    }

    texture = TextureHandle.createStatic(width, height, format, levels)

    buffer.verifyMagic("E.tx")
  }
}