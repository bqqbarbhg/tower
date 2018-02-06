package tower.engine.audio

import java.nio.ByteBuffer

import tower.engine.audio.source.VorbisSource
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
    val data = ByteBuffer.allocateDirect(dataSize)

    val src = buffer.duplicateEx
    src.limit(src.position + dataSize)
    buffer.position(buffer.position + dataSize)
    data.put(src)
    data.position(0)

    this.sampleSource = format match {
      case "OGGV" => new VorbisSource(data)
      case _ => throw new RuntimeException(s"Unexpected audio format $format")
    }

    buffer.verifyMagic("E.au")
  }

  def makeInstance(): SoundInstance = new SoundInstance(this)
}
