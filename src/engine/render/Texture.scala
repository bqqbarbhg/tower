package tower.engine.render

import tower.math._
import java.io.InputStream
import java.nio.ByteBuffer

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL12._
import org.lwjgl.opengl.GL13._
import org.lwjgl.opengl.EXTTextureCompressionS3TC._

import tower.util.Serialization.ByteBufferExtension
import tower.util.Identifier

class Texture {

  var name: Identifier = Identifier.Empty
  var width: Int = 0
  var height: Int = 0
  var numLevels: Int = 0
  var format: String = ""

  var textureHandle: Int = 0

  def load(buffer: ByteBuffer): Unit = {

    // -- Load data

    // @Serialize(s2tx)
    buffer.verifyMagic("s2tx")
    val MaxVersion = 1
    val version = buffer.getVersion(MaxVersion)

    this.name = buffer.getIdentifier()
    this.width = buffer.getInt()
    this.height = buffer.getInt()
    this.numLevels = buffer.getInt()
    this.format = buffer.getMagic()

    this.textureHandle = glGenTextures()
    glBindTexture(GL_TEXTURE_2D, textureHandle)

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR)
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR)

    assert(glGetError() == 0)

    var mipWidth = width
    var mipHeight = height
    for (level <- 0 until numLevels) {
      val dataSize = buffer.getInt()
      val data = buffer.duplicateEx
      data.limit(data.position + dataSize)
      buffer.advance(dataSize)

      if (format == "RGBA") {
        glTexImage2D(GL_TEXTURE_2D, level, GL_RGBA, mipWidth, mipHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, data)
      } else if (format == "DXT1") {
        glCompressedTexImage2D(GL_TEXTURE_2D, level, GL_COMPRESSED_RGBA_S3TC_DXT1_EXT, mipWidth, mipHeight, 0, data)
      } else if (format == "DXT5") {
        glCompressedTexImage2D(GL_TEXTURE_2D, level, GL_COMPRESSED_RGBA_S3TC_DXT5_EXT, mipWidth, mipHeight, 0, data)
      } else {
        throw new RuntimeException(s"Unsupported texture format '${format}'")
      }

      mipWidth = math.max(mipWidth / 2, 1)
      mipHeight = math.max(mipHeight / 2, 1)
      assert(glGetError() == 0)
    }

    buffer.verifyMagic("E.tx")
  }

}
