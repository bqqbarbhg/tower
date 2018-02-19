package res.output

import java.io.File
import collection.mutable.ArrayBuffer
import org.lwjgl.system.MemoryUtil

import core._
import util.BufferUtils._
import res.intermediate._
import res.runner.OutputFileWriter

object AnimationFile {

  def save(writer: OutputFileWriter, file: File, animation: Animation): Unit = {
    // @Serialize(s2an)

    val buffer = MemoryUtil.memAlloc(64*1024*1024)

    val Version = 2

    val data = new ArrayBuffer[Float]()

    buffer.putMagic("s2an")
    buffer.putVersion(Version)

    buffer.putDouble(animation.duration)

    buffer.putInt(animation.timelines.length)
    for (tl <- animation.timelines) {
      val base = data.length

      buffer.putIdentifier(tl.boneName)
      buffer.putInt(base)
      buffer.putInt(tl.rot.length)
      buffer.putInt(tl.pos.length)
      buffer.putInt(tl.size.length)

      var flags = 0
      if (tl.rot.length == 2 && (tl.rot(0).value - tl.rot(1).value).length < 0.00001) {
        flags |= 0x01
      }
      if (tl.pos.length == 2 && (tl.pos(0).value - tl.pos(1).value).length < 0.00001) {
        flags |= 0x02
      }
      if (tl.size.length == 2 && (tl.size(0).value - tl.size(1).value).length < 0.00001) {
        flags |= 0x04
      }

      buffer.putInt(flags)

      for (rot <- tl.rot) data += rot.time.toFloat
      for (rot <- tl.rot) {
        data += rot.value.x.toFloat
        data += rot.value.y.toFloat
        data += rot.value.z.toFloat
        data += rot.value.w.toFloat
      }

      for (pos <- tl.pos) data += pos.time.toFloat
      for (pos <- tl.pos) {
        data += pos.value.x.toFloat
        data += pos.value.y.toFloat
        data += pos.value.z.toFloat
      }

      for (size <- tl.size) data += size.time.toFloat
      for (size <- tl.size) {
        data += size.value.x.toFloat
        data += size.value.y.toFloat
        data += size.value.z.toFloat
      }
    }

    buffer.putInt(data.length)
    buffer.asFloatBuffer.put(data.toArray)
    buffer.position(buffer.position + data.length * 4)
    buffer.putMagic("E.an")
    buffer.finish()

    writer.writeFile(file, buffer)

    MemoryUtil.memFree(buffer)
  }
}
