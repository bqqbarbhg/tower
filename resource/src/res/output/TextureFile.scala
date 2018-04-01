package res.output

import org.lwjgl.system.MemoryUtil
import java.io.File

import res.intermediate._
import res.runner.OutputFileWriter
import core._
import util.BufferUtils._

object TextureFile {

  def save(writer: OutputFileWriter, file: File, texture: Texture): Unit = {

    // @Serialize(s2tx)

    val buffer = MemoryUtil.memAlloc(64 * 1024 * 1024)

    val readAsLinear = texture.readAsLinear.getOrElse {
      throw new RuntimeException("Trying to save Texture without sRGB info")
    }

    val Version = 1
    buffer.putMagic("s2tx")
    buffer.putVersion(Version)

    buffer.putInt(texture.width)
    buffer.putInt(texture.height)
    buffer.putInt(texture.levelData.length)
    buffer.putMagic(texture.format)

    var flags = 0x00
    if (readAsLinear) flags |= 0x01
    if (texture.noDownscale) flags |= 0x02
    buffer.putInt(flags)

    for (level <- texture.levelData) {
      val dataToCopy = level.duplicateEx
      buffer.putInt(level.remaining)
      buffer.put(dataToCopy)
    }

    buffer.putMagic("E.tx")
    buffer.finish()

    writer.writeFile(file, buffer)

    MemoryUtil.memFree(buffer)
  }
}
