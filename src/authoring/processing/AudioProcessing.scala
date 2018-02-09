package tower.authoring.processing

import tower.authoring.resource.{AudioResource, PcmResource}

object AudioProcessing {

  /**
    * Resamples PCM audio to a new sample rate.
    */
  def resample(pcm: PcmResource, sampleRate: Int): PcmResource = {
    val res = new PcmResource(pcm.name)

    val ratio = (pcm.sampleRate.toDouble / sampleRate.toDouble)

    res.numChannels = pcm.numChannels
    res.sampleRate = sampleRate
    res.numSamples = (pcm.numSamples / ratio).toInt
    res.data = pcm.data.map(chan => {
      for (i <- 0 until res.numSamples) yield {
        val float = i * ratio
        val base = float.toInt
        if (base + 1 >= pcm.numSamples) {
          chan.last
        } else {
          val beta = float % 1.0
          val alpha = 1.0 - beta
          val a = chan(base)
          val b = chan(base + 1)
          a * alpha + b * beta
        }
      }
    }.toArray)

    res
  }

  /**
    * Encodes an intermediate PcmResource into a 16-bit 'PCMW' AudioResource
    */
  def encodePcm(pcm: PcmResource): AudioResource = {
    val res = new AudioResource(pcm.name)
    res.numSamples = pcm.numSamples
    res.numChannels = pcm.numChannels
    res.sampleRate = pcm.sampleRate
    res.format = AudioResource.Format.Pcm

    res.data = {
      for (sample <- 0 until pcm.numSamples) yield {
        for (chan <- 0 until pcm.numChannels) yield {
          val v = (pcm.data(chan)(sample) * (1 << 15).toDouble)
          val s = math.min(math.max(v, Short.MinValue.toDouble), Short.MaxValue.toDouble).toShort
          Array((s & 0xFF).toByte, ((s >> 8) & 0xFF).toByte)
        }
      }
    }.flatten.flatten.toArray

    res
  }

}
