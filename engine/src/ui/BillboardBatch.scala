package ui

import java.nio.ByteBuffer
import org.lwjgl.system.MemoryUtil

import core._
import asset._
import render._
import render.VertexSpec._
import gfx._
import ui.Sprite.SpriteMap
import BillboardBatch._
import ui.Atlas.SpriteBounds

object BillboardBatch {
  val BatchMaxSprites = 128
  val FrameMaxSprites = 16*1024

  object BillboardShader extends ShaderAsset("shader/ui/billboard") {

    override object Textures extends SamplerBlock {
      val TexArray = sampler2DArray("TexArray", Sampler.RepeatTrilinear)
      val FinalTex = sampler2D("FinalTex", Sampler.RepeatTrilinear)
    }

    override object Permutations extends Shader.Permutations {
      val UseArray = both("UseArray", 0 to 1)
    }

    uniform(VertexUniform)
    object VertexUniform extends UniformBlock("BillboardVertexUniform") {
      val TexCoordScale = vec4("TexCoordScale")
    }

    uniform(MatrixUniform)
    object MatrixUniform extends UniformBlock("BillboardMatrixUniform") {
      val ViewProjection = mat4("ViewProjection")
    }
  }

  val BillboardSpec = VertexSpec(Vector(
    Attrib(3, DataFmt.F32, Identifier("Position")),
    Attrib(2, DataFmt.F32, Identifier("TexCoord")),
    Attrib(4, DataFmt.F32, Identifier("Color")), // <- Float to support HDR colors
    Attrib(1, DataFmt.I32, Identifier("Page")),
  ))

  val Shared = DynamicAsset("BillboardBatch.Shared", new BillboardBatch)
}

class BillboardBatch extends Unloadable {

  private var currentAtlas: Atlas = null
  private var currentAtlasIndex = -1

  private var vertexBuffer = VertexBuffer.createDynamic(BillboardSpec, FrameMaxSprites * 4).withLabel("SpriteBatch VB")
  private var vertexOffset = 0

  private var localBuffer = Memory.alloc(BillboardSpec.sizeInBytes * 4)
  private var gpuBuffer: ByteBuffer = null
  private var numSpritesInBatch = 0

  /** Camera position to orient the billboards to */
  var cameraPosition: Vector3 = Vector3.Zero

  /** Perspective to project the billboards to */
  var viewProjection: Matrix4 = Matrix4.Identity

  /** Draw a screen-oriented billboard */
  def drawScreen(sprite: Identifier, position: Vector3, size: Vector2, anchor: Vector2, color: Color): Unit = {
    val dir = (cameraPosition - position).normalizeOr { return }
    val right = (Vector3.Up cross dir).normalizeOr { return }
    val up = (dir cross right).normalizeOr { return }
    draw(sprite, position, up, right, size, anchor, color)
  }

  /** Draw billboard constrained to a single axis */
  def drawUp(sprite: Identifier, position: Vector3, up: Vector3, size: Vector2, anchor: Vector2, color: Color): Unit = {
    val dir = (cameraPosition - position).normalizeOr { return }
    val right = (up cross dir).normalizeOr { return }
    draw(sprite, position, up, right, size, anchor, color)
  }

  /** Draw billboard constrained to a single axis */
  def drawRight(sprite: Identifier, position: Vector3, right: Vector3, size: Vector2, anchor: Vector2, color: Color): Unit = {
    val dir = (cameraPosition - position).normalizeOr { return }
    val up = (dir cross right).normalizeOr { return }
    draw(sprite, position, up, right, size, anchor, color)
  }

  /** Draw a billboard constrained to two axes */
  def draw(sprite: Identifier, position: Vector3, up: Vector3, right: Vector3, size: Vector2, anchor: Vector2, color: Color): Unit = {
    val pair = SpriteMap.get(sprite)
    if (!pair.valid) {
      println(s"Sprite not found: $sprite")
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

    // Retrieve cropped UV coordinates
    val uvX0 = vertX0 * uvScaleX + uvBaseX
    val uvY0 = vertY0 * uvScaleY + uvBaseY
    val uvX1 = vertX1 * uvScaleX + uvBaseX
    val uvY1 = vertY1 * uvScaleY + uvBaseY

    // Adjust anchor
    vertX0 -= anchor.x.toFloat
    vertY0 -= anchor.y.toFloat
    vertX1 -= anchor.x.toFloat
    vertY1 -= anchor.y.toFloat

    val px = position.x.toFloat
    val py = position.y.toFloat
    val pz = position.z.toFloat
    val dxdx = (right.x * size.x).toFloat
    val dydx = (right.y * size.x).toFloat
    val dzdx = (right.z * size.x).toFloat
    val dxdy = (up.x * size.y).toFloat
    val dydy = (up.y * size.y).toFloat
    val dzdy = (up.z * size.y).toFloat

    val cr = color.r.toFloat
    val cg = color.g.toFloat
    val cb = color.b.toFloat
    val ca = color.a.toFloat

    localBuffer.putFloat(px + dxdx * vertX0 + dxdy * vertY0)
    localBuffer.putFloat(py + dydx * vertX0 + dydy * vertY0)
    localBuffer.putFloat(pz + dzdx * vertX0 + dzdy * vertY0)
    localBuffer.putFloat(uvX0)
    localBuffer.putFloat(uvY0)
    localBuffer.putFloat(cr)
    localBuffer.putFloat(cg)
    localBuffer.putFloat(cb)
    localBuffer.putFloat(ca)
    localBuffer.putInt(page)

    localBuffer.putFloat(px + dxdx * vertX1 + dxdy * vertY0)
    localBuffer.putFloat(py + dydx * vertX1 + dydy * vertY0)
    localBuffer.putFloat(pz + dzdx * vertX1 + dzdy * vertY0)
    localBuffer.putFloat(uvX1)
    localBuffer.putFloat(uvY0)
    localBuffer.putFloat(cr)
    localBuffer.putFloat(cg)
    localBuffer.putFloat(cb)
    localBuffer.putFloat(ca)
    localBuffer.putInt(page)

    localBuffer.putFloat(px + dxdx * vertX0 + dxdy * vertY1)
    localBuffer.putFloat(py + dydx * vertX0 + dydy * vertY1)
    localBuffer.putFloat(pz + dzdx * vertX0 + dzdy * vertY1)
    localBuffer.putFloat(uvX0)
    localBuffer.putFloat(uvY1)
    localBuffer.putFloat(cr)
    localBuffer.putFloat(cg)
    localBuffer.putFloat(cb)
    localBuffer.putFloat(ca)
    localBuffer.putInt(page)

    localBuffer.putFloat(px + dxdx * vertX1 + dxdy * vertY1)
    localBuffer.putFloat(py + dydx * vertX1 + dydy * vertY1)
    localBuffer.putFloat(pz + dzdx * vertX1 + dzdy * vertY1)
    localBuffer.putFloat(uvX1)
    localBuffer.putFloat(uvY1)
    localBuffer.putFloat(cr)
    localBuffer.putFloat(cg)
    localBuffer.putFloat(cb)
    localBuffer.putFloat(ca)
    localBuffer.putInt(page)

    localBuffer.position(0)
    val copySize = BillboardSpec.sizeInBytes * 4

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
      renderer.setTexture(BillboardShader.Textures.TexArray, currentAtlas.textureArray.get.texture)
    renderer.setTexture(BillboardShader.Textures.FinalTex, currentAtlas.lastTexture.get.texture)

    renderer.pushUniform(BillboardShader.VertexUniform, b => {
      import BillboardShader.VertexUniform._

      val texScaleX = 1.0f / currentAtlas.lastTexture.get.originalWidth.toFloat
      val texScaleY = 1.0f / currentAtlas.lastTexture.get.originalHeight.toFloat
      val texScaleZ = if (useArray) 1.0f / currentAtlas.textureArray.get.originalWidth.toFloat else 1.0f
      val texScaleW = if (useArray) 1.0f / currentAtlas.textureArray.get.originalHeight.toFloat else 1.0f

      TexCoordScale.set(b, texScaleX, texScaleY, texScaleZ, texScaleW)
    })

    renderer.pushUniform(BillboardShader.MatrixUniform, u => {
      import BillboardShader.MatrixUniform._
      ViewProjection.set(u, viewProjection)
    })

    val shader = BillboardShader.get
    shader.use(p => {
      p(BillboardShader.Permutations.UseArray) = useArray
    })

    val indexBuffer = SharedQuadIndexBuffer.get.indexBuffer
    renderer.drawElements(numSpritesInBatch * 6, indexBuffer, vertexBuffer, baseVertex = vertexOffset)

    vertexOffset += numSpritesInBatch * 4
    if ((vertexOffset + BatchMaxSprites) * 4 > vertexBuffer.numVertices) {
      vertexOffset = 0
    }

    currentAtlas = null
    currentAtlasIndex = -1
    numSpritesInBatch = 0
  }

  override def unload(): Unit = {
    Memory.free(localBuffer)
    vertexBuffer.free()
  }

}
