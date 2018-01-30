package tower.util

import java.nio.{ByteBuffer, ByteOrder}
import tower.util.Serialization.ByteBufferExtension

/**
  * Since ByteBuffers are not deterministically garbage collected it's possible to run into a
  * situation where the direct ByteBuffer memory runs out so instead of requesting a new one every
  * time let's just use one for everything!
  */
object SharedByteBuffer {
  private val buffer = ByteBuffer.allocateDirect(64 * 1024 * 1024).order(ByteOrder.LITTLE_ENDIAN)
  private var taken: Boolean = false

  /** Requests access to a shared direct ByteBuffer */
  def acquire(): ByteBuffer = {
    SharedByteBuffer.synchronized {
      assert(!taken)
      taken = true
      buffer.duplicateEx()
    }
  }

  /** Gives a shared direct ByteBuffer */
  def release(buffer: ByteBuffer): Unit = {
    SharedByteBuffer.synchronized {
      taken = false
    }
  }

}
