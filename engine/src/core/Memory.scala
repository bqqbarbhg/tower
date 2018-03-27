package core

import java.nio.ByteBuffer
import org.lwjgl.system.MemoryUtil

object Memory {

  def alloc(size: Int): ByteBuffer = MemoryUtil.memAlloc(size)
  def free(ptr: ByteBuffer): Unit = MemoryUtil.memFree(ptr)

}


