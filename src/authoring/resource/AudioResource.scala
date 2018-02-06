package tower.authoring.resource

import java.nio.ByteBuffer

object AudioResource {

  object Format {
    val Pcm = "PCMW"
    val OggVorbis = "OGGV"
  }

}

class AudioResource(name: String) extends tower.authoring.Resource(name) {

  var format: String = ""
  var sampleRate: Int = 0
  var numSamples: Int = 0
  var numChannels: Int = 0
  var data: Array[Byte] = Array(0)

}
