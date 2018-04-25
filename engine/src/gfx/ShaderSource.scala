package gfx

import java.nio.ByteBuffer
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import org.lwjgl.system.MemoryUtil

import asset._
import core._
import gfx.ShaderSource._
import render.opengl.ShaderProgramGl.SourceChunk
import util.BufferUtils._
import io.content.Package

object ShaderSource {
  private sealed abstract class ChunkRef
  private case class ChunkRefSource(index: Int) extends ChunkRef
  private case class ChunkRefImport(index: Int) extends ChunkRef

  def load(name: Identifier): Option[ShaderSource] = {
    Package.get.get(name).map(file => {
      val shaderSource = new ShaderSource(name)

      val buffer = MemoryUtil.memAlloc(file.sizeInBytes.toInt)
      val stream = file.read()
      buffer.readFrom(stream)
      buffer.finish()
      shaderSource.load(buffer)
      stream.close()
      MemoryUtil.memFree(buffer)

      shaderSource
    })
  }

  val NoChunks = Array[SourceChunk]()

  case class ShaderExtension(name: String, behavior: String)
}

class ShaderSource(val filename: Identifier) {

  var imports = Array[ShaderSourceAsset]()
  var sources = Array[String]()
var extensions = Array[ShaderExtension]()
  var sourceBaseLines = Array[Int]()
  private var chunkRefs = Array[ChunkRef]()

  def chunks: Seq[SourceChunk] = chunks(new mutable.HashSet[Identifier]())

  def chunks(importStatus: mutable.HashSet[Identifier]): Seq[SourceChunk] = chunkRefs.flatMap(ref => ref match {
    case ChunkRefSource(index) => Vector(SourceChunk(filename, sourceBaseLines(index), sources(index)))
    case ChunkRefImport(index) =>
      if (importStatus.add(imports(index).name)) {
        imports(index).get.chunks(importStatus)
      } else {
        NoChunks
      }
  })

  private def collectExtensions(set: mutable.HashSet[ShaderExtension]): Unit = {
    set ++= extensions

    chunkRefs.collect {
      case ChunkRefImport(index) =>
        imports(index).get.collectExtensions(set)
    }
  }

  def allExtensions: Seq[ShaderExtension] = {
    val set = new mutable.HashSet[ShaderExtension]()
    collectExtensions(set)
    set.toSeq.sortBy(e => (e.behavior, e.name))
  }

  def load(buffer: ByteBuffer): Unit = {
    // @Deserialize(.s2sh)

    val MaxVersion = 1
    buffer.verifyMagic("s2sh")
    val version = buffer.getVersion(MaxVersion)

    val numChunks = buffer.getInt()
    val numImports = buffer.getInt()
    val numSources = buffer.getInt()
    val numExtensions = buffer.getInt()

    sources = new Array[String](numSources)
    sourceBaseLines = new Array[Int](numSources)

    imports = Array.fill(numImports) { ShaderSourceAsset(buffer.getIdentifier()) }
    for (i <- 0 until numSources) {
      sources(i) = buffer.getString()
      sourceBaseLines(i) = buffer.getInt()
    }

    extensions = Array.fill(numExtensions) {
      val name = buffer.getString()
      val behavior = buffer.getString()
      ShaderExtension(name, behavior)
    }

    chunkRefs = Array.fill(numChunks) {
      buffer.getMagic() match {
        case ".inc" => ChunkRefImport(buffer.getInt())
        case ".src" => ChunkRefSource(buffer.getInt())
      }
    }

    buffer.verifyMagic("E.sh")
  }

}

