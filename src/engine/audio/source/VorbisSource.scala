package tower.engine.audio.source

import java.nio.{ByteBuffer, FloatBuffer}

import org.lwjgl.PointerBuffer
import tower.engine.audio.{SampleCursor, SampleSource}
import org.lwjgl.stb.STBVorbis._

class VorbisCursor(source: VorbisSource) extends SampleCursor {
  val handle = {
    val error: Array[Int] = Array(0)
    val handle = stb_vorbis_open_memory(source.data, error, null)
    if (handle == 0)
      throw new RuntimeException(s"Failed to initialize stb_vorbis: ${error(0)}")
    handle
  }

  private var aChannels = Array(0)
  private var pFrames = PointerBuffer.allocateDirect(1)

  private var sampleIndex: Int = 0
  private var numSamples: Int = 0
  private var numChannels: Int = 0
  private var channels: PointerBuffer = null

  override def read(samples: Array[Float], offset: Int, num: Int): Unit = {

    var ix = offset
    var ixEnd = ix + (num << 1)

    // Will terminate since this method is guaranteed to never be called in a way
    // that it would try to read data past the end of the buffer.
    while (true) {

      // Read data out of the current Vorbis frame
      val endSample = math.min(sampleIndex + ((ixEnd - ix) >> 1), numSamples)
      if (sampleIndex < endSample) {
        if (numChannels == 1) {
          val mono = channels.getFloatBuffer(0, numSamples)
          while (sampleIndex < endSample) {
            val sample = mono.get(sampleIndex)
            samples(ix + 0) = sample
            samples(ix + 1) = sample
            ix += 2
            sampleIndex += 1
          }
        } else {
          val left = channels.getFloatBuffer(0, numSamples)
          val right = channels.getFloatBuffer(0, numSamples)
          while (sampleIndex < endSample) {
            samples(ix + 0) = left.get(sampleIndex)
            samples(ix + 1) = right.get(sampleIndex)
            ix += 2
            sampleIndex += 1
          }
        }
      }

      // If there's more to read get the next frame and read some more
      if (ix == ixEnd) return
      numSamples = stb_vorbis_get_frame_float(handle, aChannels, pFrames)
      numChannels = aChannels(0)
      sampleIndex = 0
      channels = pFrames.getPointerBuffer(0, numChannels)
    }
  }

  override def seek(position: Int): Unit = {
    stb_vorbis_seek(handle, position)

    // Mark the buffered data as dirty
    sampleIndex = 0
    numSamples = 0
  }

  override def close(): Unit = {
    stb_vorbis_close(handle)
  }

}

class VorbisSource(val data: ByteBuffer) extends SampleSource {
  override def open(): SampleCursor = new VorbisCursor(this)
}

