package audio

import scala.collection.mutable.ArrayBuffer

class Mixer extends Input {

  /** Total volume for the output, not thread safe! */
  var volume: Float = 1.0f

  val inputs: ArrayBuffer[Input] = ArrayBuffer[Input]()

  var tempBuffer = Array[Float]()

  def add(input: Input): Unit = {
    inputs += input
  }

  def remove(input: Input): Unit = {
    inputs -= input
  }

  override def advance(data: Array[Float], offsetInFrames: Int, numFrames: Int, sampleRate: Int): Unit = {
    if (tempBuffer.length < numFrames * 2) {
      tempBuffer = new Array[Float](numFrames * 2)
    }

    java.util.Arrays.fill(data, offsetInFrames * 2, (offsetInFrames + numFrames) * 2, 0.0f)

    for (input <- inputs) {
      input.advance(tempBuffer, 0, numFrames, sampleRate)
      var srcIx = 0
      var dstIx = offsetInFrames * 2
      var dstEnd = dstIx + numFrames * 2
      while (dstIx < dstEnd) {
        data(dstIx + 0) += tempBuffer(srcIx + 0) * volume
        data(dstIx + 1) += tempBuffer(srcIx + 1) * volume
        srcIx += 2
        dstIx += 2
      }
    }
  }

}

