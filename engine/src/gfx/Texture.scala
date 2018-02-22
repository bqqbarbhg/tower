package gfx

import java.nio.ByteBuffer

import core._
import org.lwjgl.system.MemoryUtil
import render._
import util.BufferUtils._
import io.content.Package

object Texture {

  def load(name: Identifier): Option[Texture] = {
    Package.get.get(name).map(file => {
      val texture = new Texture()

      val buffer = MemoryUtil.memAlloc(file.sizeInBytes.toInt)
      val stream = file.read()
      buffer.readFrom(stream)
      buffer.finish()
      texture.load(buffer)
      stream.close()
      MemoryUtil.memFree(buffer)

      texture
    })
  }

}

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