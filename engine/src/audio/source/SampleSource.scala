package audio.source

trait SampleCursor {

  /**
    * Read frames from the sample source.
    *
    * Contract: This method _may never_ be called to read past the end of the buffer!
    *
    * @param data Interleaved stereo destination to read the data into.
    * @param offsetInFrames Number of frames (pairs of stereo samples) in to read to `data`
    * @param numFrames Number of frames (pairs of stereo samples) to read.
    */
  def read(data: Array[Float], offsetInFrames: Int, numFrames: Int): Unit

  /**
    * Seek to a sample in the source.
    *
    * @param frameIndex Absolute index of the frame to seek to.
    */
  def seek(frameIndex: Int): Unit

  /**
    * Release the resources used by the cursor.
    */
  def close(): Unit
}

trait SampleSource {

  /**
    * Create a `SampleCursor` into the sample source to read with.
    */
  def open(): SampleCursor

}

