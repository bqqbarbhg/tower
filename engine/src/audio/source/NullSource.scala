package audio.source

object NullCursor extends SampleCursor {
  def read(data: Array[Float], offsetInFrames: Int, numFrames: Int): Unit = {
    java.util.Arrays.fill(data, offsetInFrames * 2, (offsetInFrames + numFrames) * 2, 0.0f)
  }

  def seek(frameIndex: Int): Unit = { }
  def close(): Unit = { }
}

object NullSource extends SampleSource {
  def open(): SampleCursor = NullCursor
  def unload(): Unit = {
    throw new RuntimeException("NullSource should never be unloaded!")
  }
}

