package audio.effect

import audio.Input

class Limiter(val input: Input) extends Input {

  private val ChunkSize = 512
  private val chunk = new Array[Float](ChunkSize * 2 * 2)

  private var writeOffset = 0
  private var readOffset = ChunkSize

  private var prevGain = 1.0f
  private var nextGain = 1.0f
  private var gain = 1.0f
  private var gainDelta = 1.0f

  private var readPos = 0
  private var readEnd = 0

  private def findMaximumAmplitudeFromWriteChunk(): Float = {
    var max = 0.0f
    var ix = writeOffset * 2
    var end = ix + ChunkSize * 2
    while (ix < end) {
      val abs = math.abs(chunk(ix))
      if (abs > max) max = abs
      ix += 1
    }
    max
  }

  private def processChunk(sampleRate: Int): Unit = {
    input.advance(chunk, writeOffset, ChunkSize, sampleRate)

    val maxAmp = findMaximumAmplitudeFromWriteChunk()

    prevGain = nextGain
    nextGain = if (maxAmp > 0.9f) {
      0.9f / maxAmp
    } else {
      1.0f
    }

    gain = prevGain
    gainDelta = (nextGain - prevGain) / ChunkSize

    {
      var temp = writeOffset
      writeOffset = readOffset
      readOffset = temp
    }

    readPos = readOffset
    readEnd = readOffset + ChunkSize
  }

  def advance(data: Array[Float], offsetInFrames: Int, numFrames: Int, sampleRate: Int): Unit = {

    var destIx = offsetInFrames
    var destEnd = destIx + numFrames
    while (destIx < numFrames) {
      if (readPos >= readEnd) processChunk(sampleRate)

      val dstBase = destIx << 1
      val srcBase = readPos << 1
      data(dstBase + 0) = chunk(srcBase + 0) * gain
      data(dstBase + 1) = chunk(srcBase + 1) * gain
      gain += gainDelta

      readPos += 1
      destIx += 1
    }

  }

}

