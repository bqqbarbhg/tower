package ui

import render._
import asset.{DynamicAsset, Unloadable}
import org.lwjgl.system.MemoryUtil

object SharedQuadIndexBuffer extends DynamicAsset("SharedQuadIndexBuffer", new SharedQuadIndexBuffer)

class SharedQuadIndexBuffer extends Unloadable {
  val numQuads = math.max(math.max(Font.MaxQuadsPerDraw, SpriteBatch.BatchMaxSprites), BillboardBatch.BatchMaxSprites)

  val indexBuffer = {
    val data = MemoryUtil.memAlloc(numQuads * 6 * 2)
    for (i <- 0 until numQuads) {
      val base = i * 4
      data.putShort((base + 0).toShort)
      data.putShort((base + 2).toShort)
      data.putShort((base + 1).toShort)
      data.putShort((base + 1).toShort)
      data.putShort((base + 2).toShort)
      data.putShort((base + 3).toShort)
    }
    data.position(0)
    val ib = IndexBuffer.createStatic(data).withLabel("Shared quad IB")
    MemoryUtil.memFree(data)
    ib
  }

  def unload(): Unit = {
    indexBuffer.free()
  }
}


