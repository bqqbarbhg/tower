package ui

import java.nio.ByteBuffer

import org.lwjgl.system.MemoryUtil
import core._
import gfx._
import render._
import ui.Sprite.{AtlasIndexPair, SpriteMap}
import Atlas.SpriteBounds
import render.VertexSpec._
import LineBatch._
import asset.ShaderAsset
import gfx.Shader.Permutations

import scala.collection.mutable.ArrayBuffer

object LineBatch {

  val MaxVertsPerDraw = 4*1024
  val MaxVertsPerFrame = 64*1024

  object LineShader extends ShaderAsset("shader/ui/line") {

    val MaxLinesPerDraw = 16

    override object Textures extends SamplerBlock {
      val TexArray = sampler2DArray("TexArray", Sampler.RepeatBilinearNoMip)
      val FinalTex = sampler2D("FinalTex", Sampler.RepeatBilinearNoMip)
    }

    override object Permutations extends Shader.Permutations {
      val UseArray = frag("UseArray", 0 to 1)
    }

    override object Defines extends Shader.Defines {
      val MaxLines = frag("MaxLines", MaxLinesPerDraw)
    }

    uniform(LineUniform)
    object LineUniform extends UniformBlock("LineUniform") {
      val TexCoordScale = vec4("TexCoordScale")
      val PosScale = vec4("PosScale")
    }

    uniform(LineInstanceUniform)
    object LineInstanceUniform extends UniformBlock("LineInstanceUniform") {
      val TexCoords = vec4("TexCoords", MaxLinesPerDraw)
      val Pages = vec4("Pages", MaxLinesPerDraw)
    }

  }

  val SpriteSpec = VertexSpec(Vector(
    Attrib(2, DataFmt.F32, Identifier("Position")),
    Attrib(2, DataFmt.F32, Identifier("TexCoord")),
    Attrib(1, DataFmt.I32, Identifier("Index")),
  ))
}

class LineBatch {

  private var currentAtlas: Atlas = null
  private var currentAtlasIndex = -1

  private var vertexBuffer = VertexBuffer.createDynamic(SpriteSpec, MaxVertsPerFrame).withLabel("LineBatch VB")
  private var vertexOffset = 0

  private var gpuBuffer: ByteBuffer = null
  private var numVertsInBatch = 0

  private var prevPos: Vector2 = Vector2.Zero

  private var linesInBatch = new ArrayBuffer[AtlasIndexPair](LineShader.MaxLinesPerDraw)

  def draw(sprite: Identifier, segments: Seq[Vector2], width: Double): Unit = {
    if (segments.length < 2) return

    val pair = SpriteMap.get(sprite)
    if (!pair.valid) {
      println(s"Sprite not found: ${sprite}")
      return
    }

    if (currentAtlasIndex != pair.atlas || numVertsInBatch + segments.length * 2 + 1 >  MaxVertsPerDraw || linesInBatch.length >= LineShader.MaxLinesPerDraw) {
      flush()
      currentAtlasIndex = pair.atlas
      currentAtlas = SpriteMap.atlasAssets(currentAtlasIndex).get
      gpuBuffer = vertexBuffer.beginMap(vertexOffset, MaxVertsPerDraw)
    }

    val lineIndex = linesInBatch.length
    linesInBatch += pair

    val halfWidth = width * 0.5

    var prev = segments(0)
    var cur = segments(1)
    var offset = 0.0f

    var prevDiff: Vector2 = null

    {
      val diff = (cur - prev)
      val diffLen = diff.length
      val dir = diff / diffLen
      val up = dir.perpendicular * halfWidth

      val a = prev - up
      val b = prev + up

      if (numVertsInBatch > 0) {
        gpuBuffer.putFloat(prevPos.x.toFloat)
        gpuBuffer.putFloat(prevPos.y.toFloat)
        gpuBuffer.putInt(0)
        gpuBuffer.putFloat(0.0f)
        gpuBuffer.putFloat(0.0f)
        gpuBuffer.putInt(0)
      }

      gpuBuffer.putFloat(a.x.toFloat)
      gpuBuffer.putFloat(a.y.toFloat)
      gpuBuffer.putFloat(offset)
      gpuBuffer.putFloat(0.0f)
      gpuBuffer.putInt(lineIndex)

      gpuBuffer.putFloat(b.x.toFloat)
      gpuBuffer.putFloat(b.y.toFloat)
      gpuBuffer.putFloat(offset)
      gpuBuffer.putFloat(1.0f)
      gpuBuffer.putInt(lineIndex)

      offset += diffLen.toFloat
      prevDiff = diff
    }

    var ix = 2
    val ixEnd = segments.length
    while (ix < ixEnd) {
      prev = cur
      cur = segments(ix)

      val diff = (cur - prev)
      val diffLen = diff.length
      val dir = (diff + prevDiff).normalize
      val up = dir.perpendicular * halfWidth

      val a = prev - up
      val b = prev + up

      gpuBuffer.putFloat(a.x.toFloat)
      gpuBuffer.putFloat(a.y.toFloat)
      gpuBuffer.putFloat(offset)
      gpuBuffer.putFloat(0.0f)
      gpuBuffer.putInt(lineIndex)

      gpuBuffer.putFloat(b.x.toFloat)
      gpuBuffer.putFloat(b.y.toFloat)
      gpuBuffer.putFloat(offset)
      gpuBuffer.putFloat(1.0f)
      gpuBuffer.putInt(lineIndex)

      offset += diffLen.toFloat
      prevDiff = diff
      ix += 1

      prevPos = b
    }

    {
      val diff = (cur - prev)
      val diffLen = diff.length
      val dir = diff / diffLen
      val up = dir.perpendicular * halfWidth

      val a = cur - up
      val b = cur + up

      gpuBuffer.putFloat(a.x.toFloat)
      gpuBuffer.putFloat(a.y.toFloat)
      gpuBuffer.putFloat(offset)
      gpuBuffer.putFloat(0.0f)
      gpuBuffer.putInt(lineIndex)


      gpuBuffer.putFloat(b.x.toFloat)
      gpuBuffer.putFloat(b.y.toFloat)
      gpuBuffer.putFloat(offset)
      gpuBuffer.putFloat(1.0f)
      gpuBuffer.putInt(lineIndex)

      offset += diffLen.toFloat
      prevPos = b
    }

    // Degenerate segment
    if (numVertsInBatch > 0)
      numVertsInBatch += 1

    numVertsInBatch += segments.length * 2
  }

  def flush(): Unit = {
    if (currentAtlas == null) return
    if (currentAtlas.lastTexture.isEmpty) return

    val useArray = currentAtlas.textureArray.isDefined

    vertexBuffer.endMap(numVertsInBatch)

    val renderer = Renderer.get

    if (useArray)
      renderer.setTexture(LineShader.Textures.TexArray, currentAtlas.textureArray.get.texture)
    renderer.setTexture(LineShader.Textures.FinalTex, currentAtlas.lastTexture.get.texture)

    renderer.pushUniform(LineShader.LineUniform, b => {
      import LineShader.LineUniform._

      val target = renderer.currentRenderTarget
      val targetW = target.width.toFloat
      val targetH = target.height.toFloat

      val texScaleX = 1.0f / currentAtlas.lastTexture.get.originalWidth.toFloat
      val texScaleY = 1.0f / currentAtlas.lastTexture.get.originalHeight.toFloat
      val texScaleZ = if (useArray) 1.0f / currentAtlas.textureArray.get.originalWidth.toFloat else 1.0f
      val texScaleW = if (useArray) 1.0f / currentAtlas.textureArray.get.originalHeight.toFloat else 1.0f
      val screenX = 2.0f / targetW
      val screenY = -2.0f / targetH

      TexCoordScale.set(b, texScaleX, texScaleY, texScaleZ, texScaleW)
      PosScale.set(b, screenX, screenY, 0.0f, 0.0f)
    })

    renderer.pushUniform(LineShader.LineInstanceUniform, u => {
      import LineShader.LineInstanceUniform._

      var ix = 0
      while (ix < linesInBatch.length) {
        val D = currentAtlas.data
        val A = currentAtlas.spriteBase + SpriteBounds.size * linesInBatch(ix).index

        val page = SpriteBounds.Page.get(D, A).toInt

        var uvBaseX = SpriteBounds.UvBaseX.get(D, A)
        var uvBaseY = SpriteBounds.UvBaseY.get(D, A)
        var uvScaleX = SpriteBounds.UvScaleX.get(D, A)
        var uvScaleY = SpriteBounds.UvScaleY.get(D, A)

        TexCoords.set(u, ix, uvBaseX.toFloat, uvBaseY.toFloat, uvScaleX.toFloat, uvScaleY.toFloat)
        Pages.set(u, ix, page.toFloat - 1.0f, 0, 0, 0)

        ix += 1
      }
    })

    val shader = LineShader.get
    shader.use(p => {
      p(LineShader.Permutations.UseArray) = useArray
    })

    renderer.drawTriangleStrip(numVertsInBatch, vertexBuffer, baseVertex = vertexOffset)

    vertexOffset += numVertsInBatch
    if (vertexOffset + MaxVertsPerDraw > vertexBuffer.numVertices) {
      vertexOffset = 0
    }

    linesInBatch.clear()
    currentAtlas = null
    currentAtlasIndex = -1
    numVertsInBatch = 0
  }

  def unload(): Unit = {
    vertexBuffer.free()
  }

}

