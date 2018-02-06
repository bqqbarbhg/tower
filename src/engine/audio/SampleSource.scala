package tower.engine.audio

trait SampleCursor {

  /**
    * Read frames from the sample source.
    *
    * Contract: This method _may never_ be called to read past the end of the buffer!
    *
    * @param samples Interleaved stereo destination to read the data into.
    * @param offset Offset to the array to read to.
    * @param num Number of frames (pairs of stereo samples) to read.
    */
  def read(samples: Array[Float], offset: Int, num: Int): Unit

  /**
    * Seek to a sample in the source.
    *
    * @param sampleIndex Absolute index of the sample to seek to.
    */
  def seek(sampleIndex: Int): Unit

  /**
    * Release the resources used by the cursor.
    */
  def close(): Unit
}

trait SampleSource {

  /**
    * Create a _cursor_ into the sample source to read with.
    */
  def open(): SampleCursor

}
