import java.nio.ByteBuffer

import org.lwjgl.system.MemoryStack
import java.nio.ByteBuffer

package object core {

  /** Int, but semantically index of an identifier */
  type IdentifierIx = Int

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

    /** Return a version of the buffer which starts at current position with new limit */
    def limited(numBytes: Int): ByteBuffer = {
      val copy = buffer.sliceEx
      copy.limit(numBytes)
      copy
    }

    /** Return a slice of the buffer from [offset, offset + numBytes[
      * Position is guaranteed to be zero. */
    def sliced(offset: Int, numBytes: Int): ByteBuffer = {
      val copy = sliceEx
      copy.position(offset)
      copy.limit(offset + numBytes)
      copy.sliceEx
    }

    /** Return a slice of the buffer from [offset, offset + numBytes[
      * Position may be non-zero. */
    def slicedOffset(offset: Int, numBytes: Int): ByteBuffer = {
      val copy = sliceEx
      copy.position(offset)
      copy.limit(offset + numBytes)
      copy
    }

    /** Skip forward a number of bytes */
    def skip(numBytes: Int): Unit = {
      buffer.position(buffer.position + numBytes)
    }

    /** Read a sub-buffer of size `numBytes` from the buffer */
    def getBuffer(numBytes: Int): ByteBuffer = {
      val buf = buffer.limited(numBytes)
      buffer.skip(numBytes)
      buf
    }

    /** Set the limit to be current position and rewind to beginning */
    def finish(): Unit = {
      buffer.limit(buffer.position)
      buffer.position(0)
    }

  }

  def clamp(v: Int, min: Int, max: Int) = scala.math.min(scala.math.max(v, min), max)
  def clamp(v: Float, min: Float, max: Float) = scala.math.min(scala.math.max(v, min), max)
  def clamp(v: Double, min: Double, max: Double) = scala.math.min(scala.math.max(v, min), max)

  /** Code path that should be never run, behaves like assert(false) */
  def unreachable(reason: String): Nothing = {
    throw new AssertionError(s"Should never reach here: $reason")
  }
}
