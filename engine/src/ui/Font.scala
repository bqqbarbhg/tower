package ui

import java.nio.{ByteBuffer, ByteOrder}

import Font._
import gfx._
import core._
import org.lwjgl.system.MemoryUtil
import render._
import util.BufferUtils._
import io.content.Package

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

    /** Horizontal offset when rendering */
    val OffsetX = float
    /** Horizontal offset when rendering */
    val OffsetY = float

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
      Attrib(2, F32,  Identifier("Position")),
      Attrib(4, UN8,  Identifier("ChannelMask")),
      Attrib(2, UN16, Identifier("TexCoord")),
    )
  })

  val MaxQuadsPerDraw = 4 * 1024
  val MaxQuadsPerFrame = 64 * 1024

  lazy val fontIndexBuffer = IndexBuffer.createStatic(withStack {
    val data = alloca(MaxQuadsPerDraw * 6 * 2)
    for (i <- 0 until MaxQuadsPerDraw) {
      val base = i * 6
      data.putShort((base + 0).toShort)
      data.putShort((base + 2).toShort)
      data.putShort((base + 1).toShort)
      data.putShort((base + 1).toShort)
      data.putShort((base + 2).toShort)
      data.putShort((base + 3).toShort)
    }
    data.position(0)
    data
  })

  lazy val fontVertexBuffer = VertexBuffer.createDynamic(FontVertexSpec, 4 * MaxQuadsPerFrame)
  var fontVertexOffset = 0

  object FontUniform extends UniformBlock("FontUniform") {
    val TexCoordToScreen = vec4("TexCoordToScreen")
  }

  object FontPixelUniform extends UniformBlock("FontPixelUniform") {
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
    Shader.load("shader/font", FontPermutations, FontTextures, FontUniform, FontPixelUniform)
  }

  def load(name: String): Option[Font] = withStack {
    Package.get.get(name).map(file => {
      val font = new Font()

      val buffer = MemoryUtil.memAlloc(file.sizeInBytes.toInt)
      val stream = file.read()
      buffer.readFrom(stream)
      buffer.finish()
      font.load(buffer)
      stream.close()
      MemoryUtil.memFree(buffer)

      font
    })
  }

}

class Font {

  /** Actual character info data, stored away from the GC */
  var data: ByteBuffer = null

  /** Base address of the CharInfo array */
  var charInfoBase: Int = 0

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
  private def writeTextVertices(buffer: ByteBuffer, variant: Variant, string: String, position: Vector2): Int = {

    var numQuads = 0

    var posX: Float = position.x.toFloat
    var posY: Float = position.y.toFloat

    val D = this.data
    var prevCodepoint = 0
    for (char <- string) {
      val codepoint = char.toInt
      val index = findCharsetIndexFromCodepoint(codepoint)
      if (index >= 0) {
        val CA = charInfoBase + index * CharInfo.size
        val VA = variant.charDataOffset + index * VarCharInfo.size

        val kernOffset = CharInfo.KernOffset.get(D,CA)
        val kernCount = CharInfo.KernCount.get(D,CA)
        val kerning = findKerning(kernOffset, kernCount, prevCodepoint)

        val srcX0 = VarCharInfo.SrcX0.get(D,VA)
        val srcY0 = VarCharInfo.SrcY0.get(D,VA)
        val srcX1 = VarCharInfo.SrcX1.get(D,VA)
        val srcY1 = VarCharInfo.SrcY1.get(D,VA)

        val x = posX + VarCharInfo.OffsetX.get(D,VA)
        val y = posY + VarCharInfo.OffsetY.get(D,VA)
        val channel = VarCharInfo.Channel.get(D,VA)
        val mask = ChannelMasks(channel)

        if (mask >= 0) {
          posX += kerning

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

          val advance = CharInfo.Advance.get(D,CA)
          posX += advance

          numQuads += 1
        }
      }
      prevCodepoint = codepoint
    }

    numQuads
  }

  def drawText(variant: Variant, string: String, position: Vector2): Unit = {
    val renderer = Renderer.get

    val maxVerts = string.length * 4
    if (Font.fontVertexOffset + maxVerts > fontVertexBuffer.numVertices) {
      Font.fontVertexOffset = 0
    }

    var numQuads = 0
    fontVertexBuffer.map(Font.fontVertexOffset, maxVerts, buffer => {
      numQuads = writeTextVertices(buffer, variant, string, position)
      numQuads * 4
    })

    val offset = Font.fontVertexOffset
    Font.fontVertexOffset += numQuads * 4

    renderer.pushUniform(FontPixelUniform, b => {
      FontPixelUniform.Color.set(b, 1.0f, 1.0f, 1.0f, 1.0f)
    })

    renderer.pushUniform(FontUniform, b => {
      val texCoordRatioX = texture.width.toFloat / 1280.0f
      val texCoordRatioY = texture.height.toFloat / 720.0f

      FontUniform.TexCoordToScreen.set(b, texCoordRatioX, texCoordRatioY, 0.0f, 0.0f)
    })

    fontShader.use(perm => {
      perm(FontPermutations.UseSdf) = variant.useSdf
    })

    renderer.drawElements(numQuads, fontIndexBuffer, fontVertexBuffer, baseVertex = offset)
  }

  def load(buffer: ByteBuffer): Unit = {

    // @Deserialize(s2ft)
    val MaxVersion = 1
    buffer.verifyMagic("s2ft")
    val version = buffer.getVersion(MaxVersion)

    // Read the header
    val texFile = buffer.getIdentifier()
    val numCharset = buffer.getInt()
    val numKernData = buffer.getInt()
    val numVariants = buffer.getInt()

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
        CharInfo.Advance.set(D, A, buffer.getInt())
        CharInfo.LeftSideBearing.set(D, A, buffer.getInt())
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
      if ((flags & 0x01) != 0) variant.useSdf = true

      variant.height = buffer.getInt()
      variant.scale = buffer.getFloat()

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
      }
    }

    buffer.verifyMagic("E.ft")

    // Load the texture
    texture = Texture.load(texFile).get
  }

  def unload(): Unit = {
    if (data != null) {
      MemoryUtil.memFree(data)
      data = null
    }
  }

}

