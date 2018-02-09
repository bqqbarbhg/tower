package tower.engine.audio.platform

import tower.engine.audio.AudioOutput

class NullAudioOutput(val sampleRate: Int) extends AudioOutput {
  private var samplesWritten = 0

  override def write(sampleData: Array[Float], numFrames: Int): Unit = this.synchronized {
    samplesWritten += numFrames
  }
  override def currentFrame: Long = this.synchronized { samplesWritten }
  override def close(): Unit = { }
}

