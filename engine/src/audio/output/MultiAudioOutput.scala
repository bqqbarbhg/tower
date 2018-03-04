package audio.output


class MultiAudioOutput(val outputs: Seq[AudioOutput]) extends AudioOutput {

  override def open(): Unit = {
    for (output <- outputs)
      output.open()
  }

  override def close(): Unit = {
    for (output <- outputs)
      output.close()
  }

  override def start(): Unit = {
    for (output <- outputs)
      output.start()
  }

  override def write(sampleData: Array[Float], numFrames: Int): Unit = this.synchronized {
    for (output <- outputs)
      output.write(sampleData, numFrames)
  }

  override def playbackFrame: Long = outputs.head.playbackFrame

}
