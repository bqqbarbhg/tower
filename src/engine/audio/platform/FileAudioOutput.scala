package tower.engine.audio.platform

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataOutputStream, FileOutputStream}
import javax.sound.sampled._

import tower.engine.audio.AudioOutput

class FileAudioOutput(val sampleRate: Int, val filename: String) extends AudioOutput {
  private val exportStream = new DataOutputStream(new FileOutputStream(filename))

  private val chunkBuffer = new Array[Byte](8 * 1024)
  private val samplesPerChunk = chunkBuffer.length / 2

  private var samplesWritten = 0

  override def write(sampleData: Array[Float], numFrames: Int): Unit = this.synchronized {
    val numSamples = numFrames * 2
    var position = 0
    while (position < numSamples) {
      val toWrite = math.min(numSamples - position, samplesPerChunk)
      var offset = 0
      while (offset < toWrite) {
        val sample = sampleData(position + offset)
        val sampleI = (sample * 32767.0f * 0.8f).toInt
        val base = offset << 1
        chunkBuffer(base) = (sampleI & 0xFF).toByte
        chunkBuffer(base + 1) = ((sampleI >> 8) & 0xFF).toByte
        offset += 1
      }

      exportStream.write(chunkBuffer, 0, toWrite)

      position += toWrite
    }
    samplesWritten += numFrames
    exportStream.flush()
  }

  override def currentFrame: Long = samplesWritten

  override def close(): Unit = {
    exportStream.close()
  }

}

