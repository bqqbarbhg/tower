package tower.util

import java.nio.ByteBuffer
import java.nio.FloatBuffer

object BufferUtils {

  implicit class FloatBufferExtension(val buffer: FloatBuffer) extends AnyVal {
    def limited(num: Int): FloatBuffer = { val b = buffer.duplicate; b.limit(num); b }
    def offset(offset: Int): FloatBuffer = { val b = buffer.duplicate; b.position(buffer.position + offset); b }
  }

}
