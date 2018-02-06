package tower.engine.audio

/**
  * Represents a consumer that can render sound.
  */
trait AudioOutput {

  /**
    * Add audio data to output. May block if outputting too fast but that is not guaranteed.
    *
    * @param sampleData Interleaved stereo 16-bit PCM audio data
    * @param numFrames Number of frames ie. stereo sample pairs to write
    */
  def write(sampleData: Array[Short], numFrames: Int): Unit

  /**
    * Returns the position of the actual sound playback.
    */
  def currentFrame: Long

  /**
    * Closes the audio output cleanly.
    */
  def close(): Unit

}
