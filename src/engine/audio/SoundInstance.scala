package tower.engine.audio

class SoundInstance(val sound: Sound) {

  private val sampleRatio: Double = (sound.sampleRate / sound.engine.sampleRate.toDouble)

  private var pitch: Float = 1.0f
  private var volumeLeft: Float = 1.0f
  private var volumeRight: Float = 1.0f

  private val source = sound.sampleSource
  /** Stateful cursor to the sample source */
  private val cursor = source.open()

  /** Current playback time counted in the number of sample frames of the source audio */
  private var timeInSourceSamples: Double = 0.0

  private val BufferSize = 1024

  /** Local buffer of the sample contents */
  private var buffer = new Array[Float](BufferSize * 2)
  /** Index of the first valid sample contained in `buffer` */
  private var bufferFirstSample: Int = 0
  /** Index of the last valid sample contained in `buffer` */
  private var bufferLastSample: Int = 0
  /** Number of samples contained in `buffer` */
  private var bufferNumSamples: Int = 0

  /** Has the instance been recently created */
  private var atBeginning: Boolean = true

  def advance(samples: Array[Float], num: Int, startIndex: Int = 0): Unit = {

    var sampleIndex = startIndex
    while (sampleIndex < num) {
      val time = timeInSourceSamples
      val baseSample = time.toInt

      // After this `buffer` will be re-adjusted to contain the current sample
      if (baseSample < bufferFirstSample || baseSample + 1 > bufferLastSample) {
        if (baseSample + 1 < bufferLastSample + BufferSize - 1 && bufferNumSamples > 0 && baseSample >= bufferFirstSample) {
          // Easy case: Next sample is located in the next buffer that will be read
          // Move the last sample to the first in case it's needed for interpolation
          val base = (bufferNumSamples - 1) << 1
          buffer(0) = buffer(base + 0)
          buffer(1) = buffer(base + 1)
          bufferFirstSample = bufferLastSample
          val toRead = math.min(sound.lengthInSamples - (bufferLastSample + 1), BufferSize - 1)
          if (toRead <= 0) return
          cursor.read(buffer, 2, toRead)
          bufferNumSamples = toRead + 1
        } else {
          // Hard case: Need to seek to set the cursor position
          bufferFirstSample = baseSample
          bufferNumSamples = math.min(sound.lengthInSamples - bufferFirstSample, BufferSize)
          if (bufferNumSamples <= 0) return
          if (!atBeginning || baseSample != 0) cursor.seek(baseSample)
          cursor.read(buffer, 0, bufferNumSamples)
        }

        atBeginning = false
        bufferLastSample = bufferFirstSample + bufferNumSamples - 1
      }

      timeInSourceSamples += pitch * sampleRatio

      val offset = baseSample - bufferFirstSample
      val beta = (time - time.floor).toFloat
      val alpha = 1.0 - beta
      val base = offset << 1
      val al = buffer(base + 0)
      val ar = buffer(base + 1)
      val bl = buffer(base + 2)
      val br = buffer(base + 3)
      val sl = al * alpha + bl * beta
      val sr = ar * alpha + br * beta

      samples((sampleIndex << 1) + 0) += sl.toFloat * volumeLeft
      samples((sampleIndex << 1) + 1) += sr.toFloat * volumeRight
      sampleIndex += 1
    }
  }

}
