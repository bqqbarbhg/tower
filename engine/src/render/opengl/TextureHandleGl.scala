package render.opengl

import java.nio.ByteBuffer

import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.GL21._
import org.lwjgl.opengl.GL30._
import org.lwjgl.opengl.GL42._
import org.lwjgl.opengl.EXTTextureCompressionS3TC._
import org.lwjgl.opengl.EXTTextureSRGB._

object TextureHandleGl {

  /**
    * Create a texture with constant contents.
    *
    * @param width Width in pixels
    * @param height Height in pixels
    * @param format Opaque internal format
    * @param levels Mipmap level data in decreasing size
    * @param srgbToLinear If enabled, the texture fetches will interpret the data as sRGB _and_ convert it
    */
  def createStatic(width: Int, height: Int, format: String, levels: Seq[ByteBuffer], srgbToLinear: Boolean): TextureHandleGl = {
    val useSrgb = if (srgbToLinear) {
      format match {
        case "RGBA" => true
        case "DXT1" => GL.getCapabilities.GL_EXT_texture_sRGB
        case "DXT5" => GL.getCapabilities.GL_EXT_texture_sRGB
        case _ => false
      }
    } else false

    val tex = new TextureHandleGl(width, height, levels.length, format, GL_TEXTURE_2D, useSrgb)
    glBindTexture(GL_TEXTURE_2D, tex.texture)

    var w = width
    var h = height
    for ((data, level) <- levels.zipWithIndex) {
      if (useSrgb) {
        format match {
          case "RGBA" => glTexImage2D(GL_TEXTURE_2D, level, GL_SRGB8_ALPHA8, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, data)
          case "DXT1" => glCompressedTexImage2D(GL_TEXTURE_2D, level, GL_COMPRESSED_SRGB_S3TC_DXT1_EXT, w, h, 0, data)
          case "DXT5" => glCompressedTexImage2D(GL_TEXTURE_2D, level, GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT, w, h, 0, data)
        }
      } else {
        format match {
          case "RGBA" => glTexImage2D(GL_TEXTURE_2D, level, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, data)
          case "RG.." => glTexImage2D(GL_TEXTURE_2D, level, GL_RG, w, h, 0, GL_RG, GL_UNSIGNED_BYTE, data)
          case "DXT1" => glCompressedTexImage2D(GL_TEXTURE_2D, level, GL_COMPRESSED_RGBA_S3TC_DXT1_EXT, w, h, 0, data)
          case "DXT5" => glCompressedTexImage2D(GL_TEXTURE_2D, level, GL_COMPRESSED_RGBA_S3TC_DXT5_EXT, w, h, 0, data)
          case "BC4." => glCompressedTexImage2D(GL_TEXTURE_2D, level, GL_COMPRESSED_RED_RGTC1, w, h, 0, data)
          case "BC5." => glCompressedTexImage2D(GL_TEXTURE_2D, level, GL_COMPRESSED_RG_RGTC2, w, h, 0, data)
        }
      }

      w = math.max(w / 2, 1)
      h = math.max(h / 2, 1)
    }

    if (levels.length == 1) {
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR)
    }

    glBindTexture(GL_TEXTURE_2D, 0)
    tex
  }

  /**
    * Create an array texture with uninitialized contents.
    *
    * @param width Width in pixels
    * @param height Height in pixels
    * @param format Opaque internal format
    * @param numLayers Number of indices in the array
    * @param numMips Number of mipmap levels per texture in the array
    * @param srgbToLinear If enabled, the texture fetches will interpret the data as sRGB _and_ convert it
    */
  def createArray(width: Int, height: Int, format: String, numLayers: Int, numMips: Int, srgbToLinear: Boolean): TextureHandleGl = {

    // @Todo: Implement sRGB array textures
    assert(srgbToLinear == false, "Linear sRGB not implemented yet!")

    val tex = new TextureHandleGl(width, height, numMips, format, GL_TEXTURE_2D_ARRAY, false)
    glBindTexture(GL_TEXTURE_2D_ARRAY, tex.texture)

    if (OptsGl.useTexStorage && GL.getCapabilities.GL_ARB_texture_storage) {
      val internalFormat = format match {
        case "RGBA" => GL_RGBA8
        case "RG.." => GL_RG8
        case "DXT1" => GL_COMPRESSED_RGBA_S3TC_DXT1_EXT
        case "DXT5" => GL_COMPRESSED_RGBA_S3TC_DXT5_EXT
        case "BC4." => GL_COMPRESSED_RED_RGTC1
        case "BC5." => GL_COMPRESSED_RG_RGTC2
      }
      glTexStorage3D(GL_TEXTURE_2D_ARRAY, numMips, internalFormat, width, height, numLayers)
    } else {
      var w = width
      var h = height
      for (level <- 0 until numMips) {
        format match {
          case "RGBA" => glTexImage3D(GL_TEXTURE_2D_ARRAY, level, GL_RGBA, w, h, numLayers, 0, GL_RGBA, GL_UNSIGNED_BYTE, 0)
          case "RG.." => glTexImage3D(GL_TEXTURE_2D_ARRAY, level, GL_RG, w, h, numLayers, 0, GL_RG, GL_UNSIGNED_BYTE, 0)
          case "DXT1" => glCompressedTexImage3D(GL_TEXTURE_2D_ARRAY, level, GL_COMPRESSED_RGBA_S3TC_DXT1_EXT, w, h, numLayers, 0, 0, 0)
          case "DXT5" => glCompressedTexImage3D(GL_TEXTURE_2D_ARRAY, level, GL_COMPRESSED_RGBA_S3TC_DXT5_EXT, w, h, numLayers, 0, 0, 0)
          case "BC4." => glCompressedTexImage3D(GL_TEXTURE_2D_ARRAY, level, GL_COMPRESSED_RED_RGTC1, w, h, numLayers, 0, 0, 0)
          case "BC5." => glCompressedTexImage3D(GL_TEXTURE_2D_ARRAY, level, GL_COMPRESSED_RG_RGTC2, w, h, numLayers, 0, 0, 0)
        }
        w = math.max(w / 2, 1)
        h = math.max(h / 2, 1)
      }
    }

    glBindTexture(GL_TEXTURE_2D_ARRAY, 0)
    tex
  }

}

class TextureHandleGl(val width: Int, val height: Int, val numMips: Int, val format: String, val bind: Int, val hasSrgbToLinaerConversion: Boolean) {
  val texture = glGenTextures()

  /** Upload data for one of the layers an array texture */
  def setLayerData(index: Int, width: Int, height: Int, format: String, levels: Seq[ByteBuffer]): Unit = {
    assert(this.bind == GL_TEXTURE_2D_ARRAY)
    assert(this.width == width)
    assert(this.height == height)
    assert(this.format == format)
    assert(this.numMips == levels.length)

    glBindTexture(bind, texture)

    var w = width
    var h = height
    for ((data, level) <- levels.zipWithIndex) {
      format match {
        case "RGBA" => glTexSubImage3D(GL_TEXTURE_2D_ARRAY, level, 0, 0, index, w, h, 1, GL_RGBA, GL_UNSIGNED_BYTE, data)
        case "RG.." => glTexSubImage3D(GL_TEXTURE_2D_ARRAY, level, 0, 0, index, w, h, 1, GL_RG, GL_UNSIGNED_BYTE, data)
        case "DXT1" => glCompressedTexSubImage3D(GL_TEXTURE_2D_ARRAY, level, 0, 0, index, w, h, 1, GL_COMPRESSED_RGBA_S3TC_DXT1_EXT, data)
        case "DXT5" => glCompressedTexSubImage3D(GL_TEXTURE_2D_ARRAY, level, 0, 0, index, w, h, 1, GL_COMPRESSED_RGBA_S3TC_DXT5_EXT, data)
        case "BC4." => glCompressedTexSubImage3D(GL_TEXTURE_2D_ARRAY, level, 0, 0, index, w, h, 1, GL_COMPRESSED_RED_RGTC1, data)
        case "BC5." => glCompressedTexSubImage3D(GL_TEXTURE_2D_ARRAY, level, 0, 0, index, w, h, 1, GL_COMPRESSED_RG_RGTC2, data)
      }
      w = math.max(w / 2, 1)
      h = math.max(h / 2, 1)
    }

    glBindTexture(bind, 0)
  }

  def free(): Unit = {
    glDeleteTextures(texture)
  }

  def setLabel(label: String): Unit = DebugGl.setLabel(DebugGl.TEXTURE, texture, label)
}
