package ui

import java.nio.ByteBuffer

import org.lwjgl.system.MemoryUtil
import core._
import gfx._
import render._
import ui.Sprite.SpriteMap
import Atlas.SpriteBounds
import render.VertexSpec._
import SpriteBatch._
import asset.ShaderAsset
import gfx.Shader.Permutations

object SpriteBatch {

  val BatchMaxSprites = 1024
  val FrameMaxSprites = 64*1024

  object SpriteShader extends ShaderAsset("shader/sprite") {

    override object Textures extends SamplerBlock {
      val TexArray = sampler2DArray("TexArray", Sampler.ClampBilinear)
      val FinalTex = sampler2D("FinalTex", Sampler.ClampBilinear)
    }

    override object Permutations extends Shader.Permutations {
      val UseArray = both("UseArray", 0 to 1)
    }

    uniform(VertexUniform)
    object VertexUniform extends UniformBlock("SpriteVertexUniform") {
      val TexCoordScale = vec4("TexCoordScale")
      val PosScale = vec4("PosScale")
    }
  }

  // @Todo: Merge with fontIndexBuffer somehow
  lazy val spriteIndexBuffer = IndexBuffer.createStatic(withStack {
    val data = alloca(BatchMaxSprites * 6 * 2)
    for (i <- 0 until BatchMaxSprites) {
      val base = i * 4
      data.putShort((base + 0).toShort)
      data.putShort((base + 2).toShort)
      data.putShort((base + 1).toShort)
      data.putShort((base + 1).toShort)
      data.putShort((base + 2).toShort)
      data.putShort((base + 3).toShort)
    }
    data.position(0)
    data
  }).withLabel("Sprite IB")

  val SpriteSpec = VertexSpec(Vector(
    Attrib(2, DataFmt.F32, Identifier("Position")),
    //  0..11: UV X
    // 12..23: UV Y
    // 24..30: Page index
    Attrib(1, DataFmt.I32, Identifier("Packed")),
    Attrib(4, DataFmt.UN8, Identifier("Color")),
  ))
}

class SpriteBatch {

  private var currentAtlas: Atlas = null
  private var currentAtlasIndex = -1

  private var vertexBuffer = VertexBuffer.createDynamic(SpriteSpec, FrameMaxSprites * 4).withLabel("SpriteBatch VB")
  private var vertexOffset = 0

  private var localBuffer = MemoryUtil.memAlloc(SpriteSpec.sizeInBytes * 4)
  private var gpuBuffer: ByteBuffer = null
  private var numSpritesInBatch = 0

  def draw(sprite: Identifier, position: Vector2, size: Vector2, color: Color): Unit = {
    val pair = SpriteMap.get(sprite)
    if (!pair.valid) return

    if (currentAtlasIndex != pair.atlas || numSpritesInBatch == BatchMaxSprites) {
      flush()
      currentAtlasIndex = pair.atlas
      currentAtlas = SpriteMap.atlases(currentAtlasIndex)
      gpuBuffer = vertexBuffer.beginMap(vertexOffset, BatchMaxSprites * 4)
    }

    val D = currentAtlas.data
    val A = currentAtlas.spriteBase + SpriteBounds.size * pair.index
    localBuffer.position(0)

    val page = SpriteBounds.Page.get(D, A)

    val vertX0 = SpriteBounds.VertX0.get(D, A) * size.x.toFloat + position.x.toFloat
    val vertX1 = SpriteBounds.VertX1.get(D, A) * size.x.toFloat + position.x.toFloat
    val vertY0 = SpriteBounds.VertY0.get(D, A) * size.y.toFloat + position.y.toFloat
    val vertY1 = SpriteBounds.VertY1.get(D, A) * size.y.toFloat + position.y.toFloat

    val uvX0 = SpriteBounds.UvX0.get(D, A) | page << 24
    val uvX1 = SpriteBounds.UvX1.get(D, A) | page << 24
    val uvY0 = SpriteBounds.UvY0.get(D, A) << 12
    val uvY1 = SpriteBounds.UvY1.get(D, A) << 12

    val colorInt = color.toSrgbInt32

    localBuffer.putFloat(vertX0)
    localBuffer.putFloat(vertY0)
    localBuffer.putInt(uvX0 | uvY0)
    localBuffer.putInt(colorInt)

    localBuffer.putFloat(vertX1)
    localBuffer.putFloat(vertY0)
    localBuffer.putInt(uvX1 | uvY0)
    localBuffer.putInt(colorInt)

    localBuffer.putFloat(vertX0)
    localBuffer.putFloat(vertY1)
    localBuffer.putInt(uvX0 | uvY1)
    localBuffer.putInt(colorInt)

    localBuffer.putFloat(vertX1)
    localBuffer.putFloat(vertY1)
    localBuffer.putInt(uvX1 | uvY1)
    localBuffer.putInt(colorInt)

    localBuffer.position(0)
    val copySize = SpriteSpec.sizeInBytes * 4

    val oldLimit = gpuBuffer.limit()
    gpuBuffer.limit(gpuBuffer.position + copySize)
    MemoryUtil.memCopy(localBuffer, gpuBuffer)
    gpuBuffer.limit(oldLimit)
    gpuBuffer.skip(copySize)

    numSpritesInBatch += 1
  }

  def flush(): Unit = {
    if (currentAtlas == null) return
    if (currentAtlas.lastTexture.isEmpty) return

    val useArray = currentAtlas.textureArray.isDefined

    vertexBuffer.endMap(numSpritesInBatch * 4)

    val renderer = Renderer.get

    if (useArray)
      renderer.setTexture(SpriteShader.Textures.TexArray, currentAtlas.textureArray.get.texture)
    renderer.setTexture(SpriteShader.Textures.FinalTex, currentAtlas.lastTexture.get.texture)

    renderer.pushUniform(SpriteShader.VertexUniform, b => {
      import SpriteShader.VertexUniform._

      val target = renderer.currentRenderTarget
      val targetW = target.width.toFloat
      val targetH = target.height.toFloat

      val texScaleX = 1.0f / currentAtlas.lastTexture.get.width.toFloat
      val texScaleY = 1.0f / currentAtlas.lastTexture.get.height.toFloat
      val texScaleZ = if (useArray) 1.0f / currentAtlas.textureArray.get.width.toFloat else 1.0f
      val texScaleW = if (useArray) 1.0f / currentAtlas.textureArray.get.height.toFloat else 1.0f
      val screenX = 2.0f / targetW
      val screenY = -2.0f / targetH

      TexCoordScale.set(b, texScaleX, texScaleY, texScaleZ, texScaleW)
      PosScale.set(b, screenX, screenY, 0.0f, 0.0f)
    })

    val shader = SpriteShader.get
    shader.use(p => {
      p(SpriteShader.Permutations.UseArray) = useArray
    })

    renderer.drawElements(numSpritesInBatch * 6, spriteIndexBuffer, vertexBuffer, baseVertex = vertexOffset)

    vertexOffset += numSpritesInBatch * 4
    if ((vertexOffset + BatchMaxSprites) * 4 > vertexBuffer.numVertices) {
      vertexOffset = 0
    }

    currentAtlas = null
    currentAtlasIndex = -1
    numSpritesInBatch = 0
  }

  def unload(): Unit = {
    MemoryUtil.memFree(localBuffer)
  }

}

