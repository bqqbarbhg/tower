package audio.source

import core._
import java.nio.ByteBuffer
import org.lwjgl.PointerBuffer
import org.lwjgl.stb.STBVorbis._
import org.lwjgl.system.MemoryUtil

class VorbisCursor(source: VorbisSource) extends SampleCursor {
  val handle = {
    val error: Array[Int] = Array(0)
    val handle = stb_vorbis_open_memory(source.data, error, null)
    if (handle == 0)
      throw new RuntimeException(s"Failed to initialize stb_vorbis: ${error(0)}")
    handle
  }

  val ChunkSize = 2048
  private var pData = MemoryUtil.memAllocFloat(ChunkSize * source.numChannels)
  private var pFrames = MemoryUtil.memAllocPointer(source.numChannels)
  private val channels = Array.tabulate(source.numChannels)(ix => {
    val buf = pData.slice()
    buf.position(ix * ChunkSize)
    buf.limit(buf.position + ChunkSize)
    pFrames.put(ix, buf)
    buf
  })

  private var totalFrame: Int = 0
  private var bufFrame: Int = 0
  private var bufNumFrames: Int = 0
  private var atBeginning = true

  override def read(data: Array[Float], offsetInFrames: Int, numFrames: Int): Unit = {
    require(totalFrame + numFrames <= source.numFrames)
    // println(s"${offsetInFrames}-${offsetInFrames+numFrames} of ${source.numFrames}")

    var dstSample = offsetInFrames * 2
    val dstLastSample = (offsetInFrames + numFrames) * 2

    totalFrame += numFrames

    // Will terminate since this method is guaranteed to never be called in a way
    // that it would try to read data past the end of the buffer.
    while (true) {

      // Read data out of the current Vorbis frame
      var localFrame = bufFrame
      val dstLeft = (dstLastSample - dstSample) >> 1
      val endFrame = math.min(localFrame + dstLeft, bufNumFrames)
      if (localFrame < endFrame) {
        if (source.numChannels == 1) {
          val mono = channels(0)
          while (localFrame < endFrame) {
            val sample = mono.get(localFrame)
            data(dstSample + 0) = sample
            data(dstSample + 1) = sample
            dstSample += 2
            localFrame += 1
          }
        } else if (source.numChannels == 2) {
          val left = channels(0)
          val right = channels(1)
          while (localFrame < endFrame) {
            data(dstSample + 0) = left.get(localFrame)
            data(dstSample + 1) = right.get(localFrame)
            dstSample += 2
            localFrame += 1
          }
        }
        bufFrame = localFrame
      }

      // If there's more to read get the next frame and read some more
      if (dstSample == dstLastSample) return
      bufFrame = 0
      bufNumFrames = stb_vorbis_get_samples_float(handle, pFrames, ChunkSize)
    }
  }

  override def seek(position: Int): Unit = {
    if (position == totalFrame) return

    stb_vorbis_seek(handle, position)
    totalFrame = position

    // Mark the buffered data as dirty
    bufFrame = 0
    bufNumFrames = 0
  }

  override def close(): Unit = {
    stb_vorbis_close(handle)
    MemoryUtil.memFree(pFrames)
    MemoryUtil.memFree(pData)
  }
}

class VorbisSource(val data: ByteBuffer, val numFrames: Int, val numChannels: Int) extends SampleSource {
  override def open(): SampleCursor = new VorbisCursor(this)

  override def unload(): Unit = {
    MemoryUtil.memFree(data)
  }
}
