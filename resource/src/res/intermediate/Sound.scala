package res.intermediate

object Sound {
  object Format {
    val Pcm = "PCMW"
    val OggVorbis = "OGGV"
  }
}

class Sound extends Resource {
  var format: String = ""
  var sampleRate: Int = 0
  var numSamples: Int = 0
  var numChannels: Int = 0
  var data: Array[Byte] = Array()

  def unload(): Unit = {
    data = Array()
  }
}

