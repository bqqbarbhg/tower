package render.opengl

import scala.collection.mutable
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL30._

import render._
import VaoCache._

object VaoCache {

  private case class Tag(shader: Int, b0: Int, b1: Int)

  private class Entry {
    var tag: Tag = null
    var lastFrameUsed: Int = 0
    var vao: Int = 0
    var mask: Int = 0

    var prev: Entry = null
    var next: Entry = null
  }

}

/**
  * Cache that manages OpenGL Vertex Arrray Objects aka VAOs. A VAO stores the
  * vertex stream state containing the vertex format, source bufferes and
  * destination attribute locations. This means that the VAO state depends on
  * both the used vertex shader and vertex buffers.
  */
class VaoCache {

  val LruWaitFrames = 8

  private var cacheDisabledGlobalVao = 0
  private var cacheDisabledGlobalMask = 0

  private var currentFrame: Int = 0
  private val entries = new mutable.HashMap[Tag, Entry]()

  private var lruHead: Entry = null
  private var lruTail: Entry = null

  private def configureVertexBuffer(maskIn: Int, shader: ShaderProgramGl, buf: VertexBufferGl): Int = {
    import VertexSpec.DataFmt._
    var mask = 0
    val stride = buf.spec.sizeInBytes
    var offset = 0
    glBindBuffer(GL_ARRAY_BUFFER, buf.buffer)
    for (attrib <- buf.spec.attribs) {
      val ix = shader.getAttributeIndex(attrib.nameInShader)
      if (ix >= 0) {
        val bit = 1 << ix
        if ((maskIn & bit) == 0) {
          glEnableVertexAttribArray(ix)
        }
        mask |= bit
        val num = attrib.num
        attrib.fmt match {
          case F32  => glVertexAttribPointer (ix, num, GL_FLOAT,          false, stride, offset)
          case I32  => glVertexAttribIPointer(ix, num, GL_INT,                   stride, offset)
          case SN8  => glVertexAttribPointer (ix, num, GL_BYTE,           true,  stride, offset)
          case UN8  => glVertexAttribPointer (ix, num, GL_UNSIGNED_BYTE,  true,  stride, offset)
          case UI8  => glVertexAttribIPointer(ix, num, GL_BYTE,                  stride, offset)
          case SN16 => glVertexAttribPointer (ix, num, GL_SHORT,          true,  stride, offset)
          case UN16 => glVertexAttribPointer (ix, num, GL_UNSIGNED_SHORT, true,  stride, offset)
          case UF16 => glVertexAttribPointer (ix, num, GL_UNSIGNED_SHORT, false, stride, offset)
          case PAD  => // Nop: Padding is implied in `stride` and `offset`
        }
      }
      offset += attrib.sizeInBytes
    }

    mask
  }

  private def configureVao(maskIn: Int, vao: Int, shader: ShaderProgramGl, b0: VertexBufferGl, b1: VertexBufferGl): Int = {
    var mask0 = 0
    var mask1 = 0

    glBindVertexArray(vao)
    if (b0 != null) mask0 = configureVertexBuffer(maskIn, shader, b0)
    if (b1 != null) mask1 = configureVertexBuffer(maskIn, shader, b1)
    assert((mask0 & mask1) == 0, "Vertex array specified in multiple streams")

    val mask = mask0 | mask1

    var toDisable = maskIn & ~mask
    while (toDisable != 0) {
      val ix = java.lang.Integer.numberOfTrailingZeros(toDisable)
      val bit = 1 << ix
      toDisable &= ~bit
      glDisableVertexAttribArray(ix)
    }

    mask
  }

  /** Needs to be called every frame */
  def advanceFrame(): Unit = {
    currentFrame += 1
  }

  /**
    * Bind the vertex buffers `b0` and `b1` (`null` if absent) to `shader`.
    *
    * When calling this for the first time with a set of values it will create
    * and setup a new VAO for them. Later calls are guaranteed to re-use the
    * buffer unless `LruWaitFrames` has passed. If the VAO hasn't been touched
    * in `LruWaitFrames` it is possible to reclaim in a least-recently-used
    * queue.
    */
  def bindVertexBuffers(shader: ShaderProgramGl, b0: VertexBufferGl, b1: VertexBufferGl): Unit = {

    // If disabled, just update a single VAO
    if (!OptsGl.useVaoCache) {
      if (cacheDisabledGlobalVao == 0) {
        cacheDisabledGlobalVao = glGenVertexArrays()
      }
      cacheDisabledGlobalMask = configureVao(cacheDisabledGlobalMask, cacheDisabledGlobalVao, shader, b0, b1)
      return
    }

    val tag = Tag(shader.serial,
      if (b0 != null) b0.serial else 0,
      if (b1 != null) b1.serial else 0)

    val entry = entries.getOrElseUpdate(tag, {

      // Try to unqueue the last element in the LRU cache
      val tail = lruTail
      val entry = if (tail != null && currentFrame >= tail.lastFrameUsed + LruWaitFrames) {
        // Success: Unlink, remove from hash-map, and return tail
        if (tail.prev != null) tail.prev.next = null
        lruTail = tail.prev
        entries -= tail.tag
        tail
      } else {
        // Fail: Create new entry
        val entry = new Entry()
        entry.vao = glGenVertexArrays()
        entry
      }

      entry.tag = tag
      entry.mask = configureVao(entry.mask, entry.vao, shader, b0, b1)

      entry
    })

    // Unlink from LRU list and insert to the head
    if (entry.prev != null) entry.prev.next = entry.next
    if (entry.next != null) entry.next.prev = entry.prev
    if (lruHead != null) lruHead.prev = entry
    entry.next = lruHead
    lruHead = entry
    if (lruTail == null) lruTail = entry

    entry.lastFrameUsed = currentFrame
    glBindVertexArray(entry.vao)
  }

  /** Release used resources */
  def unload(): Unit = {
    for ((tag, entry) <- entries) {
      glDeleteVertexArrays(entry.vao)
    }
    entries.clear()
    lruHead = null
    lruTail = null
  }

}
