package audio

trait Input {

  /**
    * Read the contents of the sound to `data`.
    *
    * @param data Interleaved stereo sample data.
    * @param offsetInFrames Number of _frames_ to offset `data`
    * @param numFrames Number of frames to read of the data
    * @param sampleRate How many frames per second to advance
    */
  def advance(data: Array[Float], offsetInFrames: Int, numFrames: Int, sampleRate: Int): Unit

}

