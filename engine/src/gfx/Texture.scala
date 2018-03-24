package gfx

import java.nio.ByteBuffer

import core._
import org.lwjgl.system.MemoryUtil
import render._
import util.BufferUtils._
import io.content.Package
import task.Task

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

      texture.texture.setLabel(name.toString)

      texture
    })
  }

  def deferredLoad(name: Identifier): Task[Texture] = {
    val fileTask = Task.Io.add[ByteBuffer](() => {
      val file = Package.get.get(name).getOrElse {
        throw new RuntimeException(s"Asset not found: $name")
      }
      val buffer = MemoryUtil.memAlloc(file.sizeInBytes.toInt)
      val stream = file.read()
      buffer.readFrom(stream)
      buffer.finish()
      stream.close()

      buffer
    })

    val loadTask = Task.Main.add(fileTask, (buffer: ByteBuffer) => {
      val texture = new Texture()
      texture.load(buffer)
      MemoryUtil.memFree(buffer)

      texture.texture.setLabel(name.toString)

      texture
    })

    loadTask
  }

  def createArray(names: Seq[Identifier]): Option[Texture] = {
    if (names.length <= 0) return None
    val files = names.flatMap(Package.get.get(_)).toSeq
    if (files.length != names.length) return None

    val texture = new Texture()

    // Setup the array and first layer data from the first file
    {
      val file = files.head
      val buffer = MemoryUtil.memAlloc(file.sizeInBytes.toInt)
      val stream = file.read()
      buffer.readFrom(stream)
      buffer.finish()

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

      texture.texture = TextureHandle.createArray(width, height, format, files.length, numLevels, readAsLinear)
      texture.texture.setLayerData(0, width, height, format, levels)
      texture.width = width
      texture.height = height
      texture.originalWidth = width
      texture.originalHeight = height
      texture.format = format
      texture.numLevels = numLevels

      buffer.verifyMagic("E.tx")
      stream.close()
      MemoryUtil.memFree(buffer)
    }

    // Setup rest of the layers
    for ((file, index) <- files.zipWithIndex.drop(1)) {
      val buffer = MemoryUtil.memAlloc(file.sizeInBytes.toInt)
      val stream = file.read()
      buffer.readFrom(stream)
      buffer.finish()

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
      stream.close()
      MemoryUtil.memFree(buffer)
    }

    for (first <- names.headOption)
      texture.texture.setLabel(first.toString)

    Some(texture)
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