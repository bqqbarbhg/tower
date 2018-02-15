package util

import java.io.{InputStream, FileInputStream, File}
import java.nio.ByteBuffer

import org.lwjgl.BufferUtils
import org.lwjgl.system.MemoryStack
import org.lwjgl.util.xxhash._
import org.lwjgl.util.xxhash.XXHash._

object BufferHash {

  /** Calculate a 64-bit hash from a `ByteBuffer`. Includes bytes between `position`
    * and `limit`.
    */
  def hashBuffer(buf: ByteBuffer): Long = {
    val stack = MemoryStack.stackPush()
    val state = XXH64State.callocStack()
    XXH64_reset(state, 1)
    XXH64_update(state, buf)
    val hash = XXH64_digest(state)
    stack.pop()
    hash
  }

  def hashStream(stream: InputStream): Long = {
    val ChunkSize = 4096
    val array = new Array[Byte](ChunkSize)
    val stack = MemoryStack.stackPush()
    val buffer = stack.malloc(ChunkSize)

    val state = XXH64State.callocStack()
    XXH64_reset(state, 1)

    var num = stream.read(array)
    while (num > 0) {
      buffer.position(0)
      buffer.limit(ChunkSize)
      buffer.put(array)
      buffer.position(0)
      buffer.limit(num)

      XXH64_update(state, buffer)

      num = stream.read(array)
    }

    val hash = XXH64_digest(state)
    stack.pop()
    hash
  }

  def hashFile(file: File): Long = {
    val stream = new FileInputStream(file)
    val hash = hashStream(stream)
    stream.close()
    hash
  }
  def hashFile(filename: String): Long = hashFile(new File(filename))

}
