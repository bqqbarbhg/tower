package core

import java.io.ByteArrayOutputStream
import java.nio.{ByteBuffer, ByteOrder}

import util.BufferUtils._

object StackAllocator {

  /** Contains the StackAllocator for this thread */
  private val threadLocal = new ThreadLocal[StackAllocator]

  /** Get the StackAllocator for this thread */
  def get: StackAllocator = {
    val alloc = threadLocal.get
    if (alloc == null)
      throw new RuntimeException("No thread-local allocator for this thread! Call set/createForCurrentThread() first!")
    alloc
  }

  /** Sets a stack-allocator for the current thread */
  def setForCurrentThread(allocator: StackAllocator): Unit = {
    threadLocal.set(allocator)
  }

  /** Create a stack-allocator for the current thread */
  def createCurrentThread(maxSize: Int): Unit = {
    val backingStorage = ByteBuffer.allocateDirect(maxSize)
    backingStorage.order(ByteOrder.nativeOrder)
    val allocator = new StackAllocator(backingStorage)
    setForCurrentThread(allocator)
  }

}

/**
  * Allocator that allocates memory from a stack. Allocations don't need to be
  * freed but the allocations need to be contained within `push()`/`pop()` blocks.
  *
  * @param backingStorage The memory to allocate from.
  */
class StackAllocator(val backingStorage: ByteBuffer) {

  /** Maximum nested frames aka push() calls in the allocator */
  val MaxFrames = 1024

  /** Pushed stack frames */
  private val storedTop = new Array[Int](MaxFrames)

  /** Number of pushed stack frames */
  private var numFrames: Int = 0

  /** Current top of the stack */
  private var top: Int = backingStorage.position

  /** Push a new stack frame */
  def push(): Unit = {
    if (numFrames >= MaxFrames)
      throw new RuntimeException(s"Too many nested stack frames: $numFrames")

    storedTop(numFrames) = top
    numFrames += 1
  }

  /** Pop a stack frame invalidating all allocations since the previous `push()` */
  def pop(): Unit = {
    if (numFrames <= 0)
      throw new RuntimeException("No frames left to pop")

    numFrames -= 1
    top = storedTop(numFrames)
  }

  /** Allocate a block of memory from the stack. Valid until `pop()` is called */
  def allocate(numBytes: Int): ByteBuffer = {
    if (numFrames == 0)
      throw new RuntimeException("Trying to allocate memory without `push()`")
    if (numBytes < 0)
      throw new RuntimeException("Trying to allocate negative number of bytes")

    // Align up to the next 8-byte boundary
    val alignedBytes = (numBytes + 7) & ~7

    if (top + alignedBytes > backingStorage.limit)
      throw new RuntimeException(s"Stack has no space for allocation of size: $numBytes bytes")

    // Setup the resulting buffer
    val buf = backingStorage.duplicateEx
    buf.position(top)
    buf.limit(numBytes)

    top += alignedBytes

    buf
  }

}
