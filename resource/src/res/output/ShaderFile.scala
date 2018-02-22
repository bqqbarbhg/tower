package res.output

import java.io.File
import java.nio.ByteBuffer

import core._
import org.lwjgl.system.MemoryUtil
import res.intermediate._
import util.BufferUtils._
import res.runner.OutputFileWriter

object ShaderFile {

  def save(writer: OutputFileWriter, file: File, shader: Shader): Unit = {
    // @Serialize(s2sh)

    val buffer = MemoryUtil.memAlloc(64 * 1024 * 1024)

    val Version = 1
    buffer.putMagic("s2sh")
    buffer.putVersion(Version)

    buffer.putString(shader.source)

    buffer.putMagic("E.sh")
    buffer.finish()

    writer.writeFile(file, buffer)

    MemoryUtil.memFree(buffer)
  }
}

