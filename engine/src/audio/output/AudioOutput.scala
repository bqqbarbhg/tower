package audio.output

/**
  * An abstract device that audio data can be written to.
  *
  * All the API functions will be called only from the audio thread.
  */
trait AudioOutput {

  /**
    * Initialize the output.
    *
    * Guarantee: This will be called once before any other method is called.
    */
  def open(): Unit

  /**
    * Close the audio output releasing all the used resources.
    * Will block until the audio has stopped playing.
    *
    * Guarantee: This will be called once and no other method will be called after it.
    */
  def close(): Unit

  /**
    * Write sample data to the audio output.
    *
    * @param data Interleaved stereo PCM data with the range [-1.0, 1.0]
    * @param numFrames Number of pairs of stereo samples to write
    */
  def write(data: Array[Float], numFrames: Int): Unit

  /**
    * Estimate of the currently playing sample. Allowed to be behind the
    * actually playing sample but not ahead.
    */
  def playbackFrame: Long

}
