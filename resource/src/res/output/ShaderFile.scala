package res.output

import java.io.File
import java.nio.ByteBuffer

import core._
import org.lwjgl.system.MemoryUtil
import res.intermediate._
import res.intermediate.ProcessedShader._
import util.BufferUtils._
import res.runner.OutputFileWriter

object ShaderFile {

  def save(writer: OutputFileWriter, file: File, shader: ProcessedShader): Unit = {
    // @Serialize(s2sh)

    val buffer = MemoryUtil.memAlloc(64 * 1024 * 1024)

    val Version = 1
    buffer.putMagic("s2sh")
    buffer.putVersion(Version)

    val imports = shader.chunks.collect { case c: ChunkImport => c }
    val sources = shader.chunks.collect { case c: ChunkSource => c }

    buffer.putInt(shader.chunks.length)
    buffer.putInt(imports.length)
    buffer.putInt(sources.length)
    buffer.putInt(shader.extensions.length)

    for (chunk <- imports) {
      buffer.putIdentifier(chunk.filename)
    }
    for (chunk <- sources) {
      buffer.putString(chunk.source)
      buffer.putInt(chunk.baseLine)
    }
    for (ext <- shader.extensions) {
      buffer.putString(ext.name)
      buffer.putString(ext.behavior)
    }

    for (chunk <- shader.chunks) {
      val importIndex = imports.indexWhere(_ eq chunk)
      val sourceIndex = sources.indexWhere(_ eq chunk)
      if (importIndex >= 0) {
        buffer.putMagic(".inc")
        buffer.putInt(importIndex)
      } else if (sourceIndex >= 0) {
        buffer.putMagic(".src")
        buffer.putInt(sourceIndex)
      }
    }

    buffer.putMagic("E.sh")
    buffer.finish()

    writer.writeFile(file, buffer)

    MemoryUtil.memFree(buffer)
  }
}

