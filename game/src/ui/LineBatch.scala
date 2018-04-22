package ui

import java.nio.ByteBuffer

import core._
import gfx._
import render._
import ui.Sprite.{AtlasIndexPair, SpriteMap}
import Atlas.SpriteBounds
import render.VertexSpec._
import LineBatch._
import asset.ShaderAsset
import gfx.Shader.Permutations
import util.BinarySearch

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
      val PageAdvance = vec4("PageAdvance", MaxLinesPerDraw)
    }

  }

  val SpriteSpec = VertexSpec(Vector(
    Attrib(2, DataFmt.F32, Identifier("Position")),
    Attrib(2, DataFmt.F32, Identifier("TexCoord")),
    Attrib(1, DataFmt.I32, Identifier("Index")),
  ))

  case class HermiteNode(position: Vector2, tangent: Vector2)

  private case class LineInBatch(sprite: AtlasIndexPair, width: Double)

}

class LineBatch {

  private var currentAtlas: Atlas = null
  private var currentAtlasIndex = -1

  private var vertexBuffer = VertexBuffer.createDynamic(SpriteSpec, MaxVertsPerFrame).withLabel("LineBatch VB")
  private var vertexOffset = 0

  private var gpuBuffer: ByteBuffer = null
  private var numVertsInBatch = 0

  private val prevPos: UnsafeVector2 = UnsafeVector2.Zero

  private var linesInBatch = new ArrayBuffer[LineInBatch](LineShader.MaxLinesPerDraw)

  def drawHermite(sprite: Identifier, segments: Seq[HermiteNode], width: Double, minDistance: Double, maxDistance: Double, maxAngle: Double): Unit = {
    if (segments.length < 2) return

    val points = new ArrayBuffer[Vector2]()

    var prevPoint = segments(0).position
    val tangent = UnsafeVector2.Zero
    val tangentRef = UnsafeVector2.Zero
    val point = UnsafeVector2.Zero
    val next = UnsafeVector2.Zero

    points += prevPoint

    val distMaxSq = maxDistance * maxDistance
    val distMinSq = minDistance * minDistance
    val angleDotMax = math.cos(math.toRadians(maxAngle))
    val TangentStep = 0.01
    val Step = 0.1
    val SearchSteps = 64
    var t = Step
    var endT = segments.length.toDouble - 1.01

    def isConsidreableAdvance(point: UnsafeVector2, next: UnsafeVector2): Boolean = {
      if (point.distanceSquaredTo(prevPoint) <= distMinSq) return false
      if (point.distanceSquaredTo(prevPoint) >= distMaxSq) return true

      tangentRef %<- next - point
      tangent %<- point - prevPoint

      val refLen = tangentRef.length
      val tanLen = tangent.length
      if (refLen >= 0.0001 && tanLen >= 0.001) {
        tangent /= tanLen
        tangentRef /= refLen
        val dot = tangent dot tangentRef
        if (dot <= angleDotMax) return true
      }

      false
    }

    def evalPoint(pt: UnsafeVector2, unsafeT: Double): Unit = {
      val t = math.min(unsafeT, endT)
      val base = math.floor(t)
      val rel = t - base
      val ix = base.toInt

      val prev = segments(ix)
      val next = segments(ix + 1)
      Hermite.interpolate(pt, prev.position, prev.tangent, next.position, next.tangent, rel)
    }

    while (t < endT) {
      evalPoint(point, t + Step)
      evalPoint(next, t + TangentStep)

      if (isConsidreableAdvance(point, next)) {

        var ix = BinarySearch.upperBound(0, SearchSteps, index => {
          val tt = t + Step * (index.toDouble / SearchSteps.toDouble)
          evalPoint(point, tt)
          evalPoint(next, tt + TangentStep)
          isConsidreableAdvance(point, next)
        })

        t += Step * (ix.toDouble / SearchSteps.toDouble)
        evalPoint(point, t)

        val pt = point.safe
        points += pt

        prevPoint = pt
      } else {
        t += Step
      }
    }

    val lastPoint = segments.last.position
    points += lastPoint

    draw(sprite, points, width)
  }

  def draw(sprite: Identifier, segments: Seq[Vector2], width: Double): Unit = {
    if (segments.length < 2) return

    val pair = SpriteMap.get(sprite)
    if (!pair.valid) {
      println(s"Sprite not found: ${sprite}")
      return
    }

    if (currentAtlasIndex != pair.atlas || numVertsInBatch + segments.length * 2 + 2 >  MaxVertsPerDraw || linesInBatch.length >= LineShader.MaxLinesPerDraw) {
      flush()
      currentAtlasIndex = pair.atlas
      currentAtlas = SpriteMap.atlasAssets(currentAtlasIndex).get
      gpuBuffer = vertexBuffer.beginMap(vertexOffset, MaxVertsPerDraw)
    }

    val lineIndex = linesInBatch.length
    linesInBatch += LineInBatch(pair, width)

    val halfWidth = width * 0.5

    var prev = segments(0)
    var cur = segments(1)
    var offset = 0.0f

    val prevDiff = UnsafeVector2.Zero
    val diff = UnsafeVector2.Zero
    val dir = UnsafeVector2.Zero
    val up = UnsafeVector2.Zero
    val a = UnsafeVector2.Zero
    val b = UnsafeVector2.Zero
    var diffLen = 0.0

    {
      diff %<- cur - prev
      diffLen = diff.length
      dir %<- diff / diffLen
      up.perpendicularTo(dir)
      up *= halfWidth

      a %<- prev - up
      b %<- prev + up

      if (numVertsInBatch > 0) {
        gpuBuffer.putFloat(prevPos.x.toFloat)
        gpuBuffer.putFloat(prevPos.y.toFloat)
        gpuBuffer.putFloat(0.0f)
        gpuBuffer.putFloat(0.0f)
        gpuBuffer.putInt(0)

        gpuBuffer.putFloat(a.x.toFloat)
        gpuBuffer.putFloat(a.y.toFloat)
        gpuBuffer.putFloat(offset)
        gpuBuffer.putFloat(0.0f)
        gpuBuffer.putInt(lineIndex)
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
      prevDiff %<- diff
    }

    var ix = 2
    val ixEnd = segments.length
    while (ix < ixEnd) {
      prev = cur
      cur = segments(ix)

      diff %<- cur - prev
      diffLen = diff.length
      dir %<- diff + prevDiff
      dir.normalize()
      up.perpendicularTo(dir)
      up *= halfWidth

      a %<- prev - up
      b %<- prev + up

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
      ix += 1

      prevDiff %<- diff
      prevPos %<- b
    }

    {
      diff %<- (cur - prev)
      diffLen = diff.length
      dir %<- diff / diffLen
      up.perpendicularTo(dir)
      up *= halfWidth

      a %<- cur - up
      b %<- cur + up

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

      prevPos %<- b
    }

    // Degenerate segment
    if (numVertsInBatch > 0)
      numVertsInBatch += 2

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
        val line = linesInBatch(ix)

        val D = currentAtlas.data
        val A = currentAtlas.spriteBase + SpriteBounds.size * line.sprite.index

        val page = SpriteBounds.Page.get(D, A).toInt

        var uvBaseX = SpriteBounds.UvBaseX.get(D, A)
        var uvBaseY = SpriteBounds.UvBaseY.get(D, A)
        var uvScaleX = SpriteBounds.UvScaleX.get(D, A)
        var uvScaleY = SpriteBounds.UvScaleY.get(D, A)
        var aspect = SpriteBounds.Aspect.get(D, A)

        val advance = 1.0 / (line.width * aspect)

        TexCoords.set(u, ix, uvBaseX.toFloat, uvBaseY.toFloat, uvScaleX.toFloat, uvScaleY.toFloat)
        PageAdvance.set(u, ix, page.toFloat - 1.0f, advance.toFloat, 0, 0)

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

