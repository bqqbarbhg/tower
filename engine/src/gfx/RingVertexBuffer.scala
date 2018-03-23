package gfx

import core._
import render._
import asset._

class RingVertexBuffer(name: String, spec: VertexSpec, capacity: Int) extends Unloadable {
  private var lastWrapFrame = Renderer.get.frameIndex
  private var offset = 0

  /**
    * Buffer to write vertices to.
    */
  val buffer = VertexBuffer.createDynamic(spec, capacity * 4).withLabel(s"RingVertexBuffer: $name")

  /**
    * Reserve `numVertices` of space in the buffer. The current position is not advanced yet.
    *
    * @param numVertices Number of vertices to reserve
    * @return Offset into `buffer` with `numVertices` consecutive vertices available
    */
  def reserve(numVertices: Int): Int = {
    if (offset + numVertices > buffer.numVertices) {
      val frame = Renderer.get.frameIndex
      if (frame - 4 < lastWrapFrame) {
        println(s"WARNING: Ring vertex buffer wrapped in under 4 frames: $name")
      }
      offset = 0
    }

    offset
  }

  /**
    * Advance the current position by some amount of vertices. Called after `reserve()`
    * with the actual amount of written vertices.
    *
    * @param numVertices Number of vetices to advance the current position. Must be
    *                    less than or equal to the reserved amount.
    */
  def advance(numVertices: Int): Unit = {
    require(offset + numVertices <= buffer.numVertices)
    offset += numVertices
  }

  /**
    * Combination of `reserve()` and `advance()`: Push an exact amount of vertices to
    * the buffer and return their starting offset.
    *
    * @param numVertices Number of vertices to allocate from the buffer.
    * @return Offset into `buffer` with `numVertices` consecutive vertices available
    */
  def push(numVertices: Int): Int = {
    val pos = reserve(numVertices)
    advance(numVertices)
    pos
  }

  def unload(): Unit = {
    buffer.free()
  }
}

class RingVertexBufferAsset(name: String, spec: VertexSpec, capacity: Int)
  extends DynamicAsset(s"RingVertexBuffer: $name", new RingVertexBuffer(name, spec, capacity))

