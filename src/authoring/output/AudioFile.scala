package tower.authoring.output

import java.io.FileOutputStream
import java.nio.ByteBuffer

import tower.authoring.resource.AudioResource
import tower.util.Serialization.ByteBufferExtension
import tower.util.SharedByteBuffer

object AudioFile {

  def save(filename: String, audio: AudioResource): Unit = {

    // @Serialize(s2au)

    val buffer = SharedByteBuffer.acquire()

    val Version = 1
    buffer.putMagic("s2au")
    buffer.putVersion(Version)

    buffer.putIdentifier(audio.name)
    buffer.putMagic(audio.format)
    buffer.putInt(audio.numChannels)
    buffer.putInt(audio.sampleRate)
    buffer.putInt(audio.numSamples)
    buffer.putInt(audio.data.length)

    buffer.put(audio.data)

    buffer.putMagic("E.au")

    val output = new FileOutputStream(filename)
    buffer.writeTo(output)
    output.close()

    SharedByteBuffer.release(buffer)
  }
}
