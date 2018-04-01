package ui

import java.nio.{ByteBuffer, ByteOrder}

import Font._
import asset.{DynamicAsset, ShaderAsset, Unloadable}
import gfx._
import core._
import io.ContentFile
import org.lwjgl.system.MemoryUtil
import render._
import util.BufferUtils._
import io.content.Package
import task.Task

object Font {

  object CharInfo extends Struct {
    /** Amount to advance per character */
    val Advance = float
    /** Offset into the kerning table */
    val KernOffset = int
    /** Number of entries in the kern table for this character */
    val KernCount = int

    /** Amount the character spans before the current position */
    val LeftSideBearing = float
  }

  object VarCharInfo extends Struct {

    /** Source bounding box */
    val SrcX0 = short
    val SrcY0 = short
    val SrcX1 = short
    val SrcY1 = short

    /** Horizontal offset of the character quad */
    val OffsetX = float
    /** Horizontal offset of the character quad */
    val OffsetY = float
    /** Width of the character quad */
    val Width = float
    /** Height of the character quad */
    val Height = float

    /** RGBA channel in the texture */
    val Channel = byte

  }

  class Variant {
    /** Use signed distance field rendering */
    var useSdf: Boolean = false
    /** Address of the actual character data in `data` */
    var charDataOffset: Int = 0
    /** Height of the variant in pixels */
    var height: Int = 0
    /** Scaling factor for measures */
    var scale: Float = 0.0f

    /** Max outline width in pixels relative to the height of the text */
    var sdfMaxOutline: Double = 0.0
    /** Amount to step the SDF in [0.0, 1.0] */
    var sdfStep: Double = 0.0
    /** Value at the SDF edge in [0.0, 1.0] */
    var sdfEdgeValue: Double = 0.0
  }

  val FontVertexSpec = VertexSpec({
    import VertexSpec._
    import VertexSpec.DataFmt._
    Vector(
      // X/Y in screen coordinates
      Attrib(2, F32,  Identifier("Position")),
      //  0..11: UV X
      // 12..23: UV Y
      // 24..25: UV Channel
      // 26..30: Batch index
      Attrib(1, I32,  Identifier("Packed")),
    )
  })

  val MaxQuadsPerDraw = 4 * 1024
  val MaxQuadsPerFrame = 64 * 1024

  val ringBuffer = new RingVertexBufferAsset("Font", FontVertexSpec, 4 * MaxQuadsPerFrame)

  /** Number of batches that can be drawn with one draw-call */
  val MaxBatchCount: Int = 32


  object FontShader extends ShaderAsset("shader/font") {

    override object Textures extends SamplerBlock {
      val Texture = sampler2D("Texture", Sampler.ClampBilinearNoMip)
    }

    uniform(VertexUniform)
    object VertexUniform extends UniformBlock("FontVertexUniform") {
      val TexCoordScale = vec4("TexCoordScale")
      val PosScale = vec4("PosScale")
    }

    uniform(PixelUniform)
    object PixelUniform extends UniformBlock("FontPixelUniform") {
      val Color = vec4("Color", MaxBatchCount)
      val SdfRamp = vec4("SdfRamp", MaxBatchCount)
    }
  }

  def load(name: Identifier): Option[Font] = Some(deferredLoad(name).get)
  def deferredLoad(name: Identifier): Task[Font] = {
    val font = new Font()
    var texture: Texture = null

    val finishTask = Task.Main.addWithManualDependencies(1, () => {
      font.texture = texture
      font
    })

    ContentFile.load(name, buffer => {
      font.load(buffer)
      val texTask = Texture.deferredLoad(font.textureFile)
      val assignTask = Task.Main.add(texTask, (tex: Texture) => texture = tex)
      assignTask.linkDependent(finishTask)
    })

    finishTask
  }

  case class TextDraw(text: String, offset: Int, length: Int, position: Vector2, height: Double, color: Color, outline: Double, order: Int) {

    /** Sort order depending on preferred order and approximage mergability */
    def sortKey: Long = order.toLong << 32 | (color.hashCode ^ outline.hashCode ^ height.hashCode)

    /** Can `this` and `other` be drawn in a signle draw-call */
    def canMerge(other: TextDraw): Boolean = {
      // Color is uniform
      if (!color.roughlyEqual(other.color)) return false
      // Height affects variant selection and SDF properties
      if (math.abs(height - other.height) > 0.0001) return false
      // Outline changes SDF properties
      if (math.abs(outline - other.outline) > 0.0001) return false
      true
    }
  }

}

class Font {

  /** Actual character info data, stored away from the GC */
  var data: ByteBuffer = null

  /** Base address of the CharInfo array */
  var charInfoBase: Int = 0

  /** The required scaling factor for 1px high text */
  var scalePerPixelHeight: Double = 0.0

  /** Contains consecutive sorted lists of characters for kerning
    * Addressed by `CharInfo.KernOffset` */
  var kernPrevChar = Array[Int]()

  /** Number of unscaled pixels to add to advance for kerning
    * Mirrors `kernPrevChar` */
  var kernAmount = Array[Float]()

  /** Sorted array of codepoints in the charset */
  var charsetCodepoint = Array[Int]()

  /** Different sizes and rendering techniques for the font */
  var variants = Array[Variant]()

  /** Filename of the texture */
  var textureFile: Identifier = Identifier.Empty

  /** The texture data */
  var texture: Texture = null

  /** Find a character in the charset, returns -1 if not found */
  private def findCharsetIndexFromCodepoint(char: Int): Int = {
    // Inlined binary search for performance...
    var first = 0
    var count = charsetCodepoint.length
    while (count > 0) {
      val step = count >> 1
      val it = first + step
      if (charsetCodepoint(it) < char) {
        first = it + 1
        count -= step + 1
      } else {
        count = step
      }
    }

    if (first < charsetCodepoint.length && charsetCodepoint(first) == char) {
      first
    } else {
      -1
    }
  }

  /** Find a kerning amount from the kerning table.
    *
    * @param offset First index to check in the kerning table.
    * @param num Number of entries in the kerning table.
    * @param prevChar Previous character codepoint
    * @return Amount to add to the advance
    */
  private def findKerning(offset: Int, num: Int, prevChar: Int): Float = {
    if (num == 0) {
      0.0f
    } else {
      // Linear scan is fast enough here
      var ix = 0
      while (ix < num) {
        if (kernPrevChar(offset + ix) == prevChar) {
          return kernAmount(offset + ix)
        }
        ix += 1
      }
      0.0f
    }
  }

  /** Write the vertices for the characters in `string`.
    * Returns the number of written quads. */
  private def writeTextVertices(buffer: ByteBuffer, variant: Variant, draw: TextDraw, batch: Int): Int = {

    var numQuads = 0

    var posX: Float = draw.position.x.toFloat
    var posY: Float = draw.position.y.toFloat + draw.height.toFloat * 0.7f
    val advanceScale = (scalePerPixelHeight * draw.height).toFloat
    val scaleF = draw.height.toFloat / variant.height

    val batchHigh = batch << 26

    val D = this.data
    var prevCodepoint = 0

    var charIndex = 0
    while (charIndex < draw.length) {
      val char = draw.text(draw.offset + charIndex)
      val codepoint = char.toInt
      val index = findCharsetIndexFromCodepoint(codepoint)
      if (index >= 0) {
        val CA = charInfoBase + index * CharInfo.size
        val VA = variant.charDataOffset + index * VarCharInfo.size

        val kernOffset = CharInfo.KernOffset.get(D,CA)
        val kernCount = CharInfo.KernCount.get(D,CA)
        val kerning = findKerning(kernOffset, kernCount, prevCodepoint)

        val w = VarCharInfo.Width.get(D,VA) * scaleF
        val h = VarCharInfo.Height.get(D,VA) * scaleF

        val x = posX + VarCharInfo.OffsetX.get(D,VA) * scaleF
        val y = posY + VarCharInfo.OffsetY.get(D,VA) * scaleF
        val channel = VarCharInfo.Channel.get(D,VA).toInt
        val high = channel << 24 | batchHigh

        // Fold high bits into srcX0/1
        val srcX0 = VarCharInfo.SrcX0.get(D,VA).toInt | high
        val srcX1 = VarCharInfo.SrcX1.get(D,VA).toInt | high
        val srcY0 = VarCharInfo.SrcY0.get(D,VA).toInt << 12
        val srcY1 = VarCharInfo.SrcY1.get(D,VA).toInt << 12

        if (channel >= 0) {
          posX += kerning * advanceScale

          buffer.putFloat(x)
          buffer.putFloat(y)
          buffer.putInt(srcX0 | srcY0)

          buffer.putFloat(x + w)
          buffer.putFloat(y)
          buffer.putInt(srcX1 | srcY0)

          buffer.putFloat(x)
          buffer.putFloat(y + h)
          buffer.putInt(srcX0 | srcY1)

          buffer.putFloat(x + w)
          buffer.putFloat(y + h)
          buffer.putInt(srcX1 | srcY1)

          val advance = CharInfo.Advance.get(D,CA)
          posX += advance * advanceScale

          numQuads += 1
        }
      }
      prevCodepoint = codepoint
      charIndex += 1
    }

    numQuads
  }

  /** Find the most suitable variant to draw `draw` with */
  private def findVariantForDraw(draw: TextDraw): Variant = {
    if (draw.outline >= 0.001) {
      // If the draw has an outline need to select an SDF variant
      var best: Variant = null
      var bestFits = false
      for (variant <- variants) {
        if (variant.useSdf) {
          val outlineSize = variant.height.toDouble * variant.sdfMaxOutline
          val fits = outlineSize >= draw.outline

          if (!bestFits && fits) {
            // Most important: Select a variant that supports the outline thickness
            best = variant
            bestFits = true
          } else if (best != null && variant.height > best.height) {
            // Tiebreaker: Select the bigger font
            best = variant
          } else if (best == null) {
            // Lowest priority: Select something if nothing has been selected so far
            best = variant
          }
        }
      }

      assert(best != null, "Could not find an SDF font for outline")
      best
    } else {

      // Prefer exact-sized non-SDF variants
      for (variant <- variants) {
        if (!variant.useSdf && math.abs(variant.height.toDouble - draw.height) <= 0.0001)
          return variant
      }

      // Otherwise just take the biggest SDF
      {
        var best: Variant = null
        for (variant <- variants) {
          if (variant.useSdf && (best == null || variant.height > best.height))
            best = variant
        }
        if (best != null) return best
      }

      // Nothing suitable found: Just return anything
      variants.head
    }
  }

  /** Write pixel uniform data for a batch */
  private def setupPixelUniform(b: ByteBuffer, batch: Int, draw: TextDraw, variant: Variant): Unit = {
    import FontShader.PixelUniform._
    Color.setSrgb(b, batch, draw.color)

    if (variant.useSdf) {
      val scale = draw.height / variant.height
      val step = variant.sdfStep / scale
      val edge = math.max(variant.sdfEdgeValue - draw.outline * step, 0.0)

      val width = math.min(edge, step * 0.75)
      val sdfA = edge - width
      val sdfB = edge + width
      SdfRamp.set(b, batch, sdfA.toFloat, sdfB.toFloat, -123.0f, -123.0f)
    } else {
      SdfRamp.set(b, batch, -1.0f, -1.0f, -123.0f, -123.0f)
    }
  }

  /** Render `draws[offset .. offset+count[` without checking for ordering or merging.
    * Precondition: `count` is at least 1 */
  private def renderMergedDraws(draws: Seq[TextDraw], batchIndex: Array[Int], offset: Int, count: Int): Unit = {
    val renderer = Renderer.get
    val batchVariants = new Array[Variant](MaxBatchCount)

    var maxQuads = 0
    var drawnQuads = 0

    // Set the font texture
    renderer.setTexture(FontShader.Textures.Texture, texture.texture)

    // Setup the vertex uniform
    renderer.pushUniform(FontShader.VertexUniform, b => {
      import FontShader.VertexUniform._

      val target = renderer.currentRenderTarget
      val targetW = target.width.toFloat
      val targetH = target.height.toFloat

      val texScaleX = 1.0f / texture.width.toFloat
      val texScaleY = 1.0f / texture.height.toFloat
      val texCoordRatioX = targetW / texture.width.toFloat
      val texCoordRatioY = targetH / texture.height.toFloat
      val screenX = 2.0f / targetW
      val screenY = -2.0f / targetH

      TexCoordScale.set(b, texScaleX, texScaleY, texCoordRatioX, texCoordRatioY)
      PosScale.set(b, screenX, screenY, 0.0f, 0.0f)
    })

    // Resolve variants, count max vertices, setup uniforms
    renderer.pushUniform(FontShader.PixelUniform, b => {
      var prevBatch = -1
      for (i <- offset until offset + count) {
        val draw = draws(i)
        maxQuads += draw.length

        val batch = batchIndex(i)
        if (batch != prevBatch) {
          val variant = findVariantForDraw(draw)
          batchVariants(batch) = variant
          setupPixelUniform(b, batch, draw, variant)
        }
        prevBatch = batch
      }
    })

    val ringVb = ringBuffer.get
    val fontVertexBuffer = ringVb.buffer

    val vertexOffset = ringVb.reserve(maxQuads * 4)
    fontVertexBuffer.map(vertexOffset, maxQuads * 4, buffer => {
      for (i <- offset until offset + count) {
        val draw = draws(i)
        val batch = batchIndex(i)
        val variant = batchVariants(batch)
        drawnQuads += writeTextVertices(buffer, variant, draw, batch)
      }
      drawnQuads * 4
    })
    ringVb.advance(drawnQuads * 4)

    val sharedIb = SharedQuadIndexBuffer.get
    val indexBuffer = sharedIb.indexBuffer

    // Do the actual draw
    val shader = FontShader.get
    shader.use()
    renderer.drawElements(drawnQuads * 6, indexBuffer, fontVertexBuffer, baseVertex = vertexOffset)
  }

  /**
    * Get the horizontal advance from the current position to the end of a character.
    *
    * @param char Character to measure
    * @param height Height of the font in pixels
    * @param nextChar Character after this character. Specify 0 for no kerning.
    */
  def getAdvance(char: Char, height: Double, nextChar: Char = 0): Float = {
    val index = findCharsetIndexFromCodepoint(char)
    if (index < 0) return 0.0f
    val D = this.data
    val A = charInfoBase + index * CharInfo.size
    val kernCount = CharInfo.KernCount.get(D, A)
    val advance = CharInfo.Advance.get(D, A)
    val kerning = if (nextChar != 0 && kernCount > 0) {
      val index2 = findCharsetIndexFromCodepoint(nextChar)
      if (index2 >= 0) {
        val A2 = charInfoBase + index2 * CharInfo.size
        val kernOffset = CharInfo.KernOffset.get(D, A2)
        findKerning(kernOffset, kernCount, char.toInt)
      } else {
        0.0f
      }
    } else {
      0.0f
    }
    val scale = scalePerPixelHeight * height
    (advance + kerning) * scale.toFloat
  }

  /**
    * Render a collection of text draw-calls.
    */
  def render(draws: Seq[TextDraw]): Unit = {
    val orderedDraws = draws.sortBy(_.sortKey)
    val batchIndices = new Array[Int](orderedDraws.length)

    var batchBegin = 0
    var batchCount = 0

    var index = 0
    while (index < orderedDraws.length) {
      val draw = orderedDraws(index)
      if (index == 0 || !orderedDraws(index - 1).canMerge(draw)) {
        if (batchCount == MaxBatchCount) {
          renderMergedDraws(orderedDraws, batchIndices, batchBegin, index - batchBegin)
          batchBegin = index
          batchCount = 0
        }
        batchCount += 1
      }
      batchIndices(index) = batchCount - 1
      index += 1
    }

    if (index > batchBegin)
      renderMergedDraws(orderedDraws, batchIndices, batchBegin, index - batchBegin)
  }

  def load(buffer: ByteBuffer): Unit = {

    // @Deserialize(s2ft)
    val MaxVersion = 1
    buffer.verifyMagic("s2ft")
    val version = buffer.getVersion(MaxVersion)

    // Read the header
    textureFile = buffer.getIdentifier()
    val numCharset = buffer.getInt()
    val numKernData = buffer.getInt()
    val numVariants = buffer.getInt()
    scalePerPixelHeight = buffer.getDouble()

    // Setup arrays
    charsetCodepoint = new Array[Int](numCharset)
    variants = new Array[Variant](numVariants)
    kernPrevChar = new Array[Int](numKernData)
    kernAmount = new Array[Float](numKernData)

    // Setup heap data
    val layout = Struct.layout(
      CharInfo * numCharset,
      VarCharInfo * (numCharset * numVariants),
    )
    val Seq(charBase, varCharBase, dataSize) = layout
    charInfoBase = charBase
    data = MemoryUtil.memAlloc(dataSize)

    // Read charset codepoints and kern offsets
    {
      val D = data
      for (charI <- 0 until numCharset) {
        val A = charInfoBase + CharInfo.size * charI

        charsetCodepoint(charI) = buffer.getInt()
        CharInfo.Advance.set(D, A, buffer.getFloat())
        CharInfo.LeftSideBearing.set(D, A, buffer.getFloat())
        CharInfo.KernOffset.set(D, A, buffer.getInt())
        CharInfo.KernCount.set(D, A, buffer.getInt())
      }
    }

    // Read kerning info
    buffer.asIntBuffer.get(kernPrevChar)
    buffer.skip(kernPrevChar.length * 4)
    buffer.asFloatBuffer.get(kernAmount)
    buffer.skip(kernAmount.length * 4)

    // Read the variants
    for (variantI <- 0 until numVariants) {
      val variant = new Variant()
      variants(variantI) = variant

      val flags = buffer.getInt()

      variant.height = buffer.getInt()
      variant.scale = buffer.getFloat()

      if ((flags & 0x01) != 0) {
        variant.useSdf = true
        variant.sdfMaxOutline = buffer.getDouble()
        variant.sdfStep = buffer.getDouble()
        variant.sdfEdgeValue = buffer.getDouble()
      }

      val base = varCharBase + VarCharInfo.size * variantI * numCharset
      variant.charDataOffset = base

      val D = data
      for (charI <- 0 until numCharset) {
        val A = base + VarCharInfo.size * charI

        VarCharInfo.Channel.set(D, A, buffer.getInt().toByte)
        VarCharInfo.SrcX0.set(D, A, buffer.getShort())
        VarCharInfo.SrcY0.set(D, A, buffer.getShort())
        VarCharInfo.SrcX1.set(D, A, buffer.getShort())
        VarCharInfo.SrcY1.set(D, A, buffer.getShort())
        VarCharInfo.OffsetX.set(D, A, buffer.getFloat())
        VarCharInfo.OffsetY.set(D, A, buffer.getFloat())
        VarCharInfo.Width.set(D, A, buffer.getFloat())
        VarCharInfo.Height.set(D, A, buffer.getFloat())
      }
    }

    buffer.verifyMagic("E.ft")
  }

  def unload(): Unit = {
    if (texture != null) {
      texture.unload()
      texture = null
    }
    if (data != null) {
      MemoryUtil.memFree(data)
      data = null
    }
  }

}

