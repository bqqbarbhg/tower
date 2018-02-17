import java.nio.ByteBuffer

import org.lwjgl.system.MemoryStack
import java.nio.ByteBuffer

package object core {

  /** Augments code of `block` with  */
  def withStack[T](block: => T): T = {
    val alloc = StackAllocator.get
    alloc.push()
    val ret = block
    alloc.pop()
    ret
  }

  /** Allocate a `ByteBuffer` from the stack with undefined contents */
  def alloca(numBytes: Int): ByteBuffer = StackAllocator.get.allocate(numBytes)

  /** Vital ByteBuffer extensions */
  implicit class ByteBufferVitals(val buffer: ByteBuffer) extends AnyVal {

    /** For some reason JVM forgets byte order when duplicating !?! */
    def duplicateEx: ByteBuffer = buffer.duplicate.order(buffer.order)

    /** For some reason JVM forgets byte order when duplicating !?! */
    def sliceEx: ByteBuffer = buffer.slice.order(buffer.order)

    /** Set the limit to be current position and rewind to beginning */
    def finish(): Unit = {
      buffer.limit(buffer.position)
      buffer.position(0)
    }

  }

  def clamp(v: Int, min: Int, max: Int) = scala.math.min(scala.math.max(v, min), max)
  def clamp(v: Float, min: Float, max: Float) = scala.math.min(scala.math.max(v, min), max)
  def clamp(v: Double, min: Double, max: Double) = scala.math.min(scala.math.max(v, min), max)
}
