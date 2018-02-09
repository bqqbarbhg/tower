package tower.engine.audio

import java.nio.{ByteBuffer, ByteOrder}

import tower.engine.audio.source._
import tower.util.Serialization.ByteBufferExtension
import tower.util.Identifier

class Sound(val engine: AudioEngine) {

  var name: Identifier = Identifier.Empty
  var format: String = ""
  var numChannels: Int = 0
  var sampleRate: Int = 0
  var lengthInSamples: Int = 0
  var sampleSource: SampleSource = null

  def load(buffer: ByteBuffer): Unit = {

    // @Serialize(s2au)
    buffer.verifyMagic("s2au")
    val MaxVersion = 1
    val version = buffer.getVersion(MaxVersion)

    this.name = buffer.getIdentifier()
    this.format = buffer.getMagic()
    this.numChannels = buffer.getInt()
    this.sampleRate = buffer.getInt()
    this.lengthInSamples = buffer.getInt()

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
        new VorbisSource(data)
      case "PCMW" =>
        val samples = new Array[Short](this.numChannels * this.lengthInSamples)
        src.asShortBuffer.get(samples)
        new PcmSource(samples, this.numChannels)
      case _ => throw new RuntimeException(s"Unexpected audio format $format")
    }

    buffer.verifyMagic("E.au")
  }

  def makeInstance(): SoundInstance = new SoundInstance(this)
}
