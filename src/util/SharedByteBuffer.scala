package tower.util

import java.nio.ByteBuffer

object SharedByteBuffer {
  private val buffer = ByteBuffer.allocateDirect(64 * 1024 * 1024)
  private var taken: Boolean = false

  def acquire(): ByteBuffer = {
    SharedByteBuffer.synchronized {
      assert(!taken)
      taken = true
      buffer.duplicate()
    }
  }

  def release(): Unit = {
    SharedByteBuffer.synchronized {
      taken = false
    }
  }

}
