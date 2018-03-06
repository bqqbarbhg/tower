package audio.effect

import audio.Input

class SimpleLowPass(val input: Input, var decay: Double) extends Input {

  private var stateL: Float = 0.0f
  private var stateR: Float = 0.0f

  override def advance(data: Array[Float], offsetInFrames: Int, numFrames: Int, sampleRate: Int): Unit = {
    input.advance(data, offsetInFrames, numFrames, sampleRate)
    var ix = offsetInFrames * 2
    var end = ix + numFrames * 2
    val b = 1.0f - decay.toFloat
    while (ix < end) {

      stateL += b * (data(ix + 0) - stateL)
      stateR += b * (data(ix + 1) - stateR)

      data(ix + 0) = stateL
      data(ix + 1) = stateR

      ix += 2
    }
  }

}

