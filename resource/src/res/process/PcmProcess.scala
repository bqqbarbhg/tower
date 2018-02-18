package res.process

import res.intermediate._
import core._

object PcmProcess {

  /**
    * Resamples PCM audio to a new sample rate.
    */
  private def resample(pcm: PcmSound, sampleRate: Int): PcmSound = {
    val res = new PcmSound()

    val ratio = (pcm.sampleRate.toDouble / sampleRate.toDouble)

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
    * Flattens PCM sound to one channel.
    */
  private def flattenToMono(pcm: PcmSound): PcmSound = {
    val res = new PcmSound()

    res.sampleRate = pcm.sampleRate
    res.numSamples = pcm.numSamples
    res.data = Array((for (i <- 0 until res.numSamples) yield {
      val sum = (0 until pcm.numChannels).map(c => pcm.data(c)(i)).sum
      sum / res.numChannels.toDouble
    }).toArray)

    res
  }

  /**
    * Encodes an intermediate PcmSound into a 16-bit 'PCMW' Sound
    */
  private def encodePcm(pcm: PcmSound): Sound = {
    val res = new Sound()
    res.numSamples = pcm.numSamples
    res.numChannels = pcm.numChannels
    res.sampleRate = pcm.sampleRate
    res.format = Sound.Format.Pcm

    res.data = {
      for (sample <- 0 until pcm.numSamples) yield {
        for (chan <- 0 until pcm.numChannels) yield {
          val v = (pcm.data(chan)(sample) * (1 << 15).toDouble)
          val s = clamp(v, Short.MinValue.toDouble, Short.MaxValue.toDouble).toShort
          Array((s & 0xFF).toByte, ((s >> 8) & 0xFF).toByte)
        }
      }
    }.flatten.flatten.toArray

    res
  }

  /**
    * Processes a PCM sound.
    */
  def processPcm(pcmIn: PcmSound, config: Config.Res.Sound): Sound = {
    var pcm = pcmIn

    var targetSampleRate = math.min(pcm.sampleRate, config.maxSampleRate)
    if (config.sampleRate > 0) targetSampleRate = config.sampleRate

    if (targetSampleRate != pcm.sampleRate) {
      var oldPcm = pcm
      pcm = resample(pcm, targetSampleRate)
      oldPcm.unload()
    }

    if (config.mono && pcm.numChannels > 1) {
      var oldPcm = pcm
      pcm = flattenToMono(pcm)
      oldPcm.unload()
    }

    encodePcm(pcm)
  }

}

