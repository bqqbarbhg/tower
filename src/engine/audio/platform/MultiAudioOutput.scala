package tower.engine.audio.platform

import tower.engine.audio.AudioOutput

class MultiAudioOutput(val outputs: Seq[AudioOutput]) extends AudioOutput {

  override def sampleRate: Int = outputs.head.sampleRate
  override def currentFrame: Long = outputs.head.currentFrame

  override def initialize(): Unit = {
    for (output <- outputs)
      output.initialize()
  }

  override def write(sampleData: Array[Float], numFrames: Int): Unit = this.synchronized {
    for (output <- outputs)
      output.write(sampleData, numFrames)
  }

  override def close(): Unit = {
    for (output <- outputs)
      output.close()
  }

}

