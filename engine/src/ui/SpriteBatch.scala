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
      val TexArray = sampler2DArray("TexArray", Sampler.RepeatBilinear)
      val FinalTex = sampler2D("FinalTex", Sampler.RepeatBilinear)
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
    Attrib(2, DataFmt.F32, Identifier("TexCoord")),
    Attrib(4, DataFmt.UN8, Identifier("Color")),
    Attrib(1, DataFmt.I32, Identifier("Page")),
  ))

  class SpriteDraw {
    // Sprite name
    var sprite: Identifier = Identifier.Empty

    // Color
    var color: Color = Color.White

    // 2x3 transform matrix
    var m11 = 1.0f
    var m12 = 0.0f
    var m13 = 0.0f
    var m21 = 0.0f
    var m22 = 1.0f
    var m23 = 0.0f

    // Anchor point
    var anchorX = 0.0f
    var anchorY = 0.0f

    // Crop rectangle
    var cropX0 = 0.0f
    var cropY0 = 0.0f
    var cropX1 = 1.0f
    var cropY1 = 1.0f

    def copy: SpriteDraw = {
      val sd = new SpriteDraw
      sd.sprite = sprite
      sd.color = color
      sd.m11 = m11
      sd.m12 = m12
      sd.m13 = m13
      sd.m21 = m21
      sd.m22 = m22
      sd.m23 = m23
      sd.anchorX = anchorX
      sd.anchorY = anchorY
      sd.cropX0 = cropX0
      sd.cropY0 = cropY0
      sd.cropX1 = cropX1
      sd.cropY1 = cropY1
      sd
    }
  }
}

class SpriteBatch {

  private var currentAtlas: Atlas = null
  private var currentAtlasIndex = -1

  private var vertexBuffer = VertexBuffer.createDynamic(SpriteSpec, FrameMaxSprites * 4).withLabel("SpriteBatch VB")
  private var vertexOffset = 0

  private var localBuffer = MemoryUtil.memAlloc(SpriteSpec.sizeInBytes * 4)
  private var gpuBuffer: ByteBuffer = null
  private var numSpritesInBatch = 0

  /**
    * Draw a generic sprite.
    */
  def draw(pos: SpriteDraw): Unit = {
    val pair = SpriteMap.get(pos.sprite)
    if (!pair.valid) {
      println(s"Sprite not found: ${pos.sprite}")
      return
    }

    if (currentAtlasIndex != pair.atlas || numSpritesInBatch == BatchMaxSprites) {
      flush()
      currentAtlasIndex = pair.atlas
      currentAtlas = SpriteMap.atlasAssets(currentAtlasIndex).get
      gpuBuffer = vertexBuffer.beginMap(vertexOffset, BatchMaxSprites * 4)
    }

    val D = currentAtlas.data
    val A = currentAtlas.spriteBase + SpriteBounds.size * pair.index
    localBuffer.position(0)

    val page = SpriteBounds.Page.get(D, A).toInt

    var vertX0 = SpriteBounds.VertX0.get(D, A)
    var vertX1 = SpriteBounds.VertX1.get(D, A)
    var vertY0 = SpriteBounds.VertY0.get(D, A)
    var vertY1 = SpriteBounds.VertY1.get(D, A)

    var uvBaseX = SpriteBounds.UvBaseX.get(D, A)
    var uvBaseY = SpriteBounds.UvBaseY.get(D, A)
    var uvScaleX = SpriteBounds.UvScaleX.get(D, A)
    var uvScaleY = SpriteBounds.UvScaleY.get(D, A)

    // Crop
    vertX0 = math.max(pos.cropX0, vertX0)
    vertY0 = math.max(pos.cropY0, vertY0)
    vertX1 = math.min(pos.cropX1, vertX1)
    vertY1 = math.min(pos.cropY1, vertY1)

    // Cropped out of existence
    if (vertX1 <= vertX0 || vertY1 <= vertY0) return

    // Retrieve cropped UV coordinates
    val uvX0 = vertX0 * uvScaleX + uvBaseX
    val uvY0 = vertY0 * uvScaleY + uvBaseY
    val uvX1 = vertX1 * uvScaleX + uvBaseX
    val uvY1 = vertY1 * uvScaleY + uvBaseY

    // Adjust anchor
    vertX0 -= pos.anchorX
    vertY0 -= pos.anchorY
    vertX1 -= pos.anchorX
    vertY1 -= pos.anchorY

    val colorInt = pos.color.toSrgbInt32

    val xx0 = vertX0*pos.m11 + pos.m13
    val xx1 = vertX1*pos.m11 + pos.m13
    val yy0 = vertY0*pos.m22 + pos.m23
    val yy1 = vertY1*pos.m22 + pos.m23

    val xy0 = vertY0*pos.m12
    val xy1 = vertY1*pos.m12
    val yx0 = vertX0*pos.m21
    val yx1 = vertX1*pos.m21

    localBuffer.putFloat(xx0 + xy0)
    localBuffer.putFloat(yx0 + yy0)
    localBuffer.putFloat(uvX0)
    localBuffer.putFloat(uvY0)
    localBuffer.putInt(colorInt)
    localBuffer.putInt(page)

    localBuffer.putFloat(xx1 + xy0)
    localBuffer.putFloat(yx1 + yy0)
    localBuffer.putFloat(uvX1)
    localBuffer.putFloat(uvY0)
    localBuffer.putInt(colorInt)
    localBuffer.putInt(page)

    localBuffer.putFloat(xx0 + xy1)
    localBuffer.putFloat(yx0 + yy1)
    localBuffer.putFloat(uvX0)
    localBuffer.putFloat(uvY1)
    localBuffer.putInt(colorInt)
    localBuffer.putInt(page)

    localBuffer.putFloat(xx1 + xy1)
    localBuffer.putFloat(yx1 + yy1)
    localBuffer.putFloat(uvX1)
    localBuffer.putFloat(uvY1)
    localBuffer.putInt(colorInt)
    localBuffer.putInt(page)

    localBuffer.position(0)
    val copySize = SpriteSpec.sizeInBytes * 4

    val oldLimit = gpuBuffer.limit()
    gpuBuffer.limit(gpuBuffer.position + copySize)
    MemoryUtil.memCopy(localBuffer, gpuBuffer)
    gpuBuffer.limit(oldLimit)
    gpuBuffer.skip(copySize)

    numSpritesInBatch += 1
  }

  def draw(sprite: Identifier, position: Vector2, scale: Vector2, color: Color): Unit = {
    val sd = new SpriteDraw()
    sd.sprite = sprite
    sd.color = color
    sd.m13 = position.x.toFloat
    sd.m23 = position.y.toFloat
    sd.m11 = scale.x.toFloat
    sd.m22 = scale.y.toFloat
    draw(sd)
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
    vertexBuffer.free()
  }

}

