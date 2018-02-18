package res.output

import java.io.File
import org.lwjgl.system.MemoryUtil

import core._
import util.BufferUtils._
import res.intermediate._
import res.runner.OutputFileWriter

object AudioFile {

  def save(writer: OutputFileWriter, file: File, sound: Sound): Unit = {

    // @Serialize(s2au)

    val buffer = MemoryUtil.memAlloc(64*1024*1024)

    val Version = 1
    buffer.putMagic("s2au")
    buffer.putVersion(Version)

    buffer.putMagic(sound.format)
    buffer.putInt(sound.numChannels)
    buffer.putInt(sound.sampleRate)
    buffer.putInt(sound.numSamples)
    buffer.putInt(sound.data.length)

    buffer.put(sound.data)

    buffer.putMagic("E.au")

    buffer.finish()

    writer.writeFile(file, buffer)

    MemoryUtil.memFree(buffer)
  }
}