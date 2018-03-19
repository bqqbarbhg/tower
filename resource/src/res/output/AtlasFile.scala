package res.output

import java.io.File
import org.lwjgl.system.MemoryUtil

import core._
import util.BufferUtils._
import res.intermediate._
import res.runner.OutputFileWriter

object AtlasFile {

  def save(writer: OutputFileWriter, file: File, atlas: Atlas, pageNameBase: String): Unit = {
    // @Serialize(s2au)

    val buffer = MemoryUtil.memAlloc(64*1024*1024)

    val Version = 1
    buffer.putMagic("s2at")
    buffer.putVersion(Version)

    val numPages = atlas.pages.length
    buffer.putInt(atlas.locations.length)
    buffer.putInt(numPages)

    for ((loc, sprite) <- (atlas.locations zip atlas.spriteImages)) {
      buffer.putIdentifier(sprite.name)
      buffer.putInt(loc.page)
      buffer.putShort(loc.rect.x.toShort)
      buffer.putShort(loc.rect.y.toShort)
      buffer.putShort(loc.rect.w.toShort)
      buffer.putShort(loc.rect.h.toShort)
      buffer.putShort((sprite.bounds.x - sprite.imageBounds.x).toShort)
      buffer.putShort((sprite.bounds.y - sprite.imageBounds.y).toShort)
      buffer.putShort(sprite.imageBounds.w.toShort)
      buffer.putShort(sprite.imageBounds.h.toShort)
    }

    for (page <- 0 until numPages) {
      val name = s"${pageNameBase}_$page.s2tx"
      buffer.putIdentifier(name)
    }

    buffer.putMagic("E.at")
    buffer.finish()

    writer.writeFile(file, buffer)

    MemoryUtil.memFree(buffer)
  }
}

