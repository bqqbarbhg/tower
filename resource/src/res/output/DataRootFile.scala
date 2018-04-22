package res.output

import java.io.File
import org.lwjgl.system.MemoryUtil

import core._
import util.BufferUtils._
import res.runner.OutputFileWriter

object DataRootFile {

  def save(writer: OutputFileWriter, file: File): Unit = {
    // @Serialize(s2dr)

    val buffer = MemoryUtil.memAlloc(64*1024*1024)

    val Version = 1
    buffer.putMagic("s2dr")
    buffer.putVersion(Version)

    buffer.putMagic("E.dr")

    buffer.finish()

    writer.writeFile(file, buffer)

    MemoryUtil.memFree(buffer)

  }

}
