package ui

import java.nio.{ByteBuffer, ByteOrder}

import Font._
import gfx._
import core._
import render._

object Font {

  object CharInfo extends Struct {

    /** Offset into the kerning table */
    val KernOffset = int
    /** Number of entries in the kern table for this character */
    val KernCount = int

    /** Source bounding box */
    val SrcX0 = short
    val SrcY0 = short
    val SrcX1 = short
    val SrcY1 = short

    /** Horizontal offset when rendering */
    val OffsetX = float
    /** Horizontal offset when rendering */
    val OffsetY = float

    /** How much to advance per character */
    val Advance = float

    /** RGBA channel in the texture */
    val Channel = byte

  }

  class Variant {
    /** Address of the actual character data in `data` */
    var charDataOffset: Int
  }

  /** Mask to use for RGBA channels, the integer depends on endianness */
  val ChannelMasks = if (ByteOrder.nativeOrder() == ByteOrder.LITTLE_ENDIAN) {
    Array(0x000000FF, 0x0000FF00, 0x00FF0000, 0xFF000000)
  } else {
    Array(0xFF000000, 0x00FF0000, 0x0000FF00, 0x000000FF)
  }

  val FontVertexSpec = VertexSpec({
    import VertexSpec._
    import VertexSpec.DataFmt._
    Vector(
      Attrib(2, F32,  "Position"),
      Attrib(4, UN8,  "ChannelMask"),
      Attrib(2, UN16, "TexCoord"),
    )
  })

  lazy val fontVertexBuffer = VertexBuffer.createDynamic(FontVertexSpec, 4 * 64 * 1024)
  var fontVertexOffset = 0

  object FontUniform extends UniformBlock {
    val CharScale = vec4("CharScale")
  }

  object FontPixelUniform extends UniformBlock {
    val Color = vec4("Color")
  }

  object FontTextures extends SamplerBlock {
    val Texture = sampler2D("Texture", Sampler.ClampBilinearNoMip)
  }

  object FontPermutations extends Shader.Permutations {
    val UseSdf = frag("UseSdf", 0 to 1)
  }

  lazy val fontShader = {
    import Shader._
    Shader.load("font", FontPermutations, FontTextures, FontUniform, FontPixelUniform)
  }

}

class Font {

  /** Actual character info data, stored away from the GC */
  var data: ByteBuffer = null

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

  /** Find a character in the charset, returns -1 if not found */
  def findCharsetIndexFromCodepoint(char: Int): Int = {
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
  def findKerning(offset: Int, num: Int, prevChar: Int): Float = {
    if (num == 0) {
      0.0f
    } else {
      // Linear scan is fast enough here
      var ix = 0
      while (ix < num) {
        if (kernPrevChar(offset + ix) == prevChar) {
          return kernAmount(offset + ix)
        }
      }
      0.0f
    }
  }

  /** Write the vertices for the characters in `string`.
    * Returns the number of written quads. */
  def writeTextVertices(buffer: ByteBuffer, variant: Variant, string: String, position: Vector2): Int = {

    var numQuads = 0

    var posX: Float = position.x.toFloat
    var posY: Float = position.y.toFloat

    val D = this.data
    var prevCodepoint = 0
    for (char <- string) {
      val codepoint = char.toInt
      val index = findCharsetIndexFromCodepoint(codepoint)
      if (index >= 0) {
        val A = variant.charDataOffset + index * CharInfo.size

        val kernOffset = CharInfo.KernOffset.get(D,A)
        val kernCount = CharInfo.KernCount.get(D,A)
        val kerning = findKerning(kernOffset, kernCount, prevCodepoint)
        posX += kerning

        val srcX0 = CharInfo.SrcX0.get(D,A)
        val srcY0 = CharInfo.SrcY0.get(D,A)
        val srcX1 = CharInfo.SrcX1.get(D,A)
        val srcY1 = CharInfo.SrcY1.get(D,A)

        val x = posX + CharInfo.OffsetX.get(D,A)
        val y = posY + CharInfo.OffsetY.get(D,A)
        val channel = CharInfo.Channel.get(D,A)
        val mask = ChannelMasks(channel)

        // Note: x/y/mask stay constant, could be instanced?

        buffer.putFloat(x)
        buffer.putFloat(y)
        buffer.putInt(mask)
        buffer.putShort(srcX0)
        buffer.putShort(srcY0)

        buffer.putFloat(x)
        buffer.putFloat(y)
        buffer.putInt(mask)
        buffer.putShort(srcX1)
        buffer.putShort(srcY0)

        buffer.putFloat(x)
        buffer.putFloat(y)
        buffer.putInt(mask)
        buffer.putShort(srcX0)
        buffer.putShort(srcY1)

        buffer.putFloat(x)
        buffer.putFloat(y)
        buffer.putInt(mask)
        buffer.putShort(srcX1)
        buffer.putShort(srcY1)

        val advance = CharInfo.Advance.get(D,A)
        posX += advance

        numQuads += 1
      }
      prevCodepoint = codepoint
    }

    numQuads
  }

  def drawText(variant: Variant, string: String, position: Vector2): Unit = {
    val renderer = Renderer.get

    renderer.pushUniform(FontUniform, b => {
      FontUniform.CharScale.set(b, variant.scaleX, variant.scaleY, 0.0f, 0.0f)
    })

    val maxVerts = string.length * 4
    if (fontVertexOffset + maxVerts > fontVertexBuffer.numVertices) {
      fontVertexOffset = 0
    }

    var numQuads = 0
    fontVertexBuffer.map(fontVertexOffset, maxVerts, buffer => {
      numQuads = writeTextVertices(buffer, variant, string, position)
      numQuads * 4
    })

    val offset = fontVertexOffset
    fontVertexOffset += numVerts

    shader.use(perm => {
      perm(FontPermutations.UseSdf) = 0
    })

    renderer.drawElements(numQuads, fontIndexBuffer, fontVertexBuffer, baseVertex = offset)
  }

}

