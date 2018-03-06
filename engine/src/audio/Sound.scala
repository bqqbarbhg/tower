package audio

import java.nio.{ByteBuffer, ByteOrder}
import org.lwjgl.system.MemoryUtil

import audio.source.SampleSource
import core._
import util.BufferUtils._
import io.content.Package

object Sound {
  def load(name: Identifier): Option[Sound] = {
    Package.get.get(name).map(file => {
      val sound = new Sound()

      val buffer = MemoryUtil.memAlloc(file.sizeInBytes.toInt)
      val stream = file.read()
      buffer.readFrom(stream)
      buffer.finish()
      sound.load(buffer)
      stream.close()
      MemoryUtil.memFree(buffer)

      sound
    })
  }
}

class Sound {
  var format: String = ""
  var numChannels: Int = 0
  var sampleRate: Int = 0
  var lengthInFrames: Int = 0
  var sampleSource: SampleSource = null

  def load(buffer: ByteBuffer): Unit = {

    // @Serialize(s2au)
    buffer.verifyMagic("s2au")
    val MaxVersion = 1
    val version = buffer.getVersion(MaxVersion)

    this.format = buffer.getMagic()
    this.numChannels = buffer.getInt()
    this.sampleRate = buffer.getInt()
    this.lengthInFrames = buffer.getInt()

    val dataSize = buffer.getInt()

    val src = buffer.duplicateEx
    src.limit(src.position + dataSize)
    buffer.position(buffer.position + dataSize)

    this.sampleSource = format match {
      case "OGGV" =>
        val data = ByteBuffer.allocateDirect(dataSize)
        data.order(ByteOrder.LITTLE_ENDIAN)
        data.put(src)
        data.position(0)
        new source.VorbisSource(data)
      case "PCMW" =>
        val samples = new Array[Short](this.numChannels * this.lengthInFrames)
        src.asShortBuffer.get(samples)
        new source.PcmSource(samples, this.numChannels)
      case _ => throw new RuntimeException(s"Unexpected audio format $format")
    }

    buffer.verifyMagic("E.au")
  }
}