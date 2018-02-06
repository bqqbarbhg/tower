package tower.engine.audio.platform

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataOutputStream, FileOutputStream}
import javax.sound.sampled._

import tower.engine.audio.AudioOutput

class JavaAudioOutput(val sampleRate: Int) extends AudioOutput {

  private val Format = new AudioFormat(sampleRate.toFloat, 16, 2, true, false)
  private val dataLineInfo: DataLine.Info = new DataLine.Info(classOf[SourceDataLine], Format)
  private val sourceDataLine = AudioSystem.getLine(dataLineInfo).asInstanceOf[SourceDataLine]

  private val chunkBuffer = new Array[Byte](8 * 1024)
  private val samplesPerChunk = chunkBuffer.length / 2

  private var closed: Boolean = false

  sourceDataLine.open(Format)
  sourceDataLine.start()

  override def write(sampleData: Array[Short], numFrames: Int): Unit = this.synchronized {
    if (closed) return

    val numSamples = numFrames * 2
    var position = 0
    while (position < samplesToWrite) {
      val toWrite = math.min(numSamples * 2 - position, samplesPerChunk)
      var offset = 0
      while (offset < toWrite) {
        val sample = sampleData(position + offset)
        val base = offset << 1
        chunkBuffer(base) = sample.toByte
        chunkBuffer(base + 1) = (sample >> 8).toByte
        offset += 1
      }

      sourceDataLine.write(chunkBuffer, 0, toWrite * 2)

      position += toWrite
    }
  }

  override def currentFrame: Long = this.synchronized { sourceDataLine.getLongFramePosition }

  override def close(): Unit = this.synchronized {
    sourceDataLine.drain()
    sourceDataLine.close()
    closed = true
  }
}
