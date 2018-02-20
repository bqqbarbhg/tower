package render.opengl

import java.nio.ByteBuffer

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.EXTTextureCompressionS3TC._

object TextureHandleGl {

  /**
    * Create a texture with constant contents.
    *
    * @param width Width in pixels
    * @param height Height in pixels
    * @param format Opaque internal format
    * @param levels Mipmap level data in decreasing size
    */
  def createStatic(width: Int, height: Int, format: String, levels: Seq[ByteBuffer]): TextureHandleGl = {
    val tex = new TextureHandleGl(width, height)
    glBindTexture(GL_TEXTURE_2D, tex.texture)

    var w = width
    var h = height
    for ((data, level) <- levels.zipWithIndex) {
      format match {
        case "RGBA" => glTexImage2D(GL_TEXTURE_2D, level, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, data)
        case "DXT1" => glCompressedTexImage2D(GL_TEXTURE_2D, level, GL_COMPRESSED_RGBA_S3TC_DXT1_EXT, w, h, 0, data)
        case "DXT5" => glCompressedTexImage2D(GL_TEXTURE_2D, level, GL_COMPRESSED_RGBA_S3TC_DXT5_EXT, w, h, 0, data)
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

}

class TextureHandleGl(val width: Int, val height: Int) {
  val texture = glGenTextures()

}
