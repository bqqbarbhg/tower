package gfx

import java.nio.ByteBuffer

import core._
import io.ContentFile
import org.lwjgl.system.MemoryUtil
import render._
import util.BufferUtils._
import io.content.Package
import task.Task

object Texture {

  def load(name: Identifier): Option[Texture] = Some(deferredLoad(name).get)

  def deferredLoad(name: Identifier): Task[Texture] = {
    ContentFile.load(name, buffer => {
      val texture = new Texture()

      texture.load(buffer)
      texture.texture.setLabel(name.toString)

      texture
    })
  }

  def loadArray(names: Seq[Identifier]): Option[Texture] = Some(deferredLoadArray(names).get)

  def deferredLoadArray(names: Seq[Identifier]): Task[Texture] = {
    val texture = new Texture()

    ContentFile.load[Unit](names.head, (buffer: ByteBuffer) => {
      // Setup the array and first layer data from the first file
      val MaxVersion = 1
      buffer.verifyMagic("s2tx")
      val version = buffer.getVersion(MaxVersion)
      val width = buffer.getInt()
      val height = buffer.getInt()
      val numLevels = buffer.getInt()
      val format = buffer.getMagic()
      val flags = buffer.getInt()

      val readAsLinear = (flags & 0x01) != 0

      val levels = Seq.fill(numLevels) {
        val size = buffer.getInt()
        buffer.getBuffer(size)
      }

      texture.texture = TextureHandle.createArray(width, height, format, names.length, numLevels, readAsLinear)
      texture.texture.setLayerData(0, width, height, format, levels)
      texture.width = width
      texture.height = height
      texture.originalWidth = width
      texture.originalHeight = height
      texture.format = format
      texture.numLevels = numLevels

      texture.texture.setLayerData(0, width, height, format, levels)

      buffer.verifyMagic("E.tx")
    })

    // Setup rest of the layers
    for ((name, index) <- names.zipWithIndex.drop(1)) {
      ContentFile.load(name, buffer => {
        val MaxVersion = 1
        buffer.verifyMagic("s2tx")
        val version = buffer.getVersion(MaxVersion)
        val width = buffer.getInt()
        val height = buffer.getInt()
        val numLevels = buffer.getInt()
        val format = buffer.getMagic()
        val flags = buffer.getInt()

        val levels = Seq.fill(numLevels) {
          val size = buffer.getInt()
          buffer.getBuffer(size)
        }

        texture.texture.setLayerData(index, width, height, format, levels)

        buffer.verifyMagic("E.tx")
        MemoryUtil.memFree(buffer)
      })
    }

    Task.Main.add(() => {
      for (first <- names.headOption)
        texture.texture.setLabel(first.toString)
      texture
    })
  }

  def createRgba(width: Int, height: Int, content: ByteBuffer, srgbToLinear: Boolean): Texture = {
    val texture = new Texture()
    texture.width = width
    texture.height = height
    texture.format = "RGBA"
    texture.numLevels = 1
    texture.texture = TextureHandle.createStatic(width, height, "RGBA", Array(content), srgbToLinear)
    texture.texture.setLabel("CreatedRgba")
    texture
  }
}

class Texture {

  var width: Int = 0
  var height: Int = 0
  var originalWidth: Int = 0
  var originalHeight: Int = 0
  var numLevels: Int = 0
  var format: String = ""
  var texture: TextureHandle = null
  var flags: Int = 0

  def load(buffer: ByteBuffer): Unit = {
    // @Deserialize(s2tx)

    val MaxVersion = 1
    buffer.verifyMagic("s2tx")
    val version = buffer.getVersion(MaxVersion)

    width = buffer.getInt()
    height = buffer.getInt()
    numLevels = buffer.getInt()
    format = buffer.getMagic()
    flags = buffer.getInt()

    originalWidth = width
    originalHeight = height

    val readAsLinear = (flags & 0x01) != 0
    val noDownscale = (flags & 0x02) != 0

    var levels = Seq.fill(numLevels) {
      val size = buffer.getInt()
      buffer.getBuffer(size)
    }

    if (!noDownscale) {
      val maxSize = OptsGfx.maxTextureSize
      while (levels.size > 1 && (width > maxSize || height > maxSize) && (width >= 16 && height >= 16)) {
        width /= 2
        height /= 2
        levels = levels.drop(1)
      }
    }

    texture = TextureHandle.createStatic(width, height, format, levels, readAsLinear)

    buffer.verifyMagic("E.tx")
  }

  def unload(): Unit = {
    texture.free()
  }
}