package audio.output

import core._
import java.io.FileOutputStream

class FileAudioOutput(val filename: String) extends AudioOutput {
  private var stream: FileOutputStream = null
  private var chunkBuffer: Array[Byte] = null

  val FramesPerChunk = 4*1024
  val SamplesPerChunk = FramesPerChunk * 2

  override def open(): Unit = {
    stream = new FileOutputStream(filename)
    chunkBuffer = new Array[Byte](SamplesPerChunk)
  }

  override def close(): Unit = {
    stream.close()
    stream = null
    chunkBuffer = null
  }

  override def start(): Unit = { }

  override def write(data: Array[Float], numFrames: Int): Unit = {
    val numSamples = numFrames * 2
    var position = 0
    while (position < numSamples) {
      val samplesToWrite = math.min(numSamples - position, SamplesPerChunk)
      var offset = 0
      while (offset < samplesToWrite) {
        val sample = data(position + offset)
        val sampleI = clamp((sample * 32767.0f).toInt, -32767, 32767)
        val base = offset << 1
        chunkBuffer(base) = (sampleI & 0xFF).toByte
        chunkBuffer(base + 1) = ((sampleI >> 8) & 0xFF).toByte
        offset += 1
      }
      stream.write(chunkBuffer, 0, samplesToWrite * 2)
      position += samplesToWrite
    }
    stream.flush()
  }

  override def playbackFrame: Long = {
    throw new UnsupportedOperationException("File output doesn't track time")
  }
}