package res.output

import java.io.File
import org.lwjgl.system.MemoryUtil

import core._
import util.BufferUtils._
import res.intermediate._
import res.runner.OutputFileWriter

object AtlasFile {

  def save(writer: OutputFileWriter, file: File, atlas: Atlas, pageNameBase: String, spriteNames: Seq[String]): Unit = {
    // @Serialize(s2au)

    assert(atlas.locations.length == spriteNames.length)

    val buffer = MemoryUtil.memAlloc(64*1024*1024)

    val Version = 1
    buffer.putMagic("s2at")
    buffer.putVersion(Version)

    val numPages = atlas.pages.length
    buffer.putInt(atlas.locations.length)
    buffer.putInt(numPages)

    for ((loc, sprite, name) <- (atlas.locations, atlas.spriteImages, spriteNames).zipped) {
      buffer.putIdentifier(name)
      buffer.putInt(loc.page)
      buffer.putInt(loc.rect.x)
      buffer.putInt(loc.rect.y)
      buffer.putInt(loc.rect.w)
      buffer.putInt(loc.rect.h)
      buffer.putInt(sprite.bounds.x)
      buffer.putInt(sprite.bounds.y)
      buffer.putInt(sprite.image.width)
      buffer.putInt(sprite.image.height)
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

