package platform

import scala.collection.mutable.ArrayBuffer
import java.nio.ByteBuffer

import core._
import util.BufferUtils._
import io.ContentFile
import org.lwjgl.glfw.GLFWImage
import org.lwjgl.system.MemoryUtil
import task.Task

object IconImage {

  def deferredLoad(names: Iterable[Identifier]): Task[IconImage] = {
    val icon = new IconImage(names.size)

    val icons = for ((name, index) <- names.zipWithIndex) yield {
      ContentFile.load(name, Task.Io, buffer => {
        icon.loadVariant(index, buffer)
      })
    }

    val resultTask = Task.Main.addWithManualDependencies(icons.size, () => icon)
    for (task <- icons) task.linkDependent(resultTask)
    resultTask
  }
}

class IconImage(val numImages: Int) {

  val images = GLFWImage.malloc(numImages)
  val datas = new Array[ByteBuffer](numImages)

  def loadVariant(index: Int, buffer: ByteBuffer): Unit = {
    val image = images.get(index)
    val MaxVersion = 1
    buffer.verifyMagic("s2tx")
    val version = buffer.getVersion(MaxVersion)

    val width = buffer.getInt()
    val height = buffer.getInt()
    val numLevels = buffer.getInt()
    val format = buffer.getMagic()
    val flags = buffer.getInt()

    assert(numLevels == 1)
    assert(format == "RGBA")
    assert(flags == 0x02)

    val dataSize = buffer.getInt()
    assert(dataSize == width * height * 4)

    val src = buffer.getBuffer(dataSize)
    val dst = Memory.alloc(dataSize)
    MemoryUtil.memCopy(src, dst)
    datas(index) = dst

    image.width(width)
    image.height(height)
    image.pixels(dst)
  }

  def unload(): Unit = {
    for (data <- datas)
      Memory.free(data)
  }

}

