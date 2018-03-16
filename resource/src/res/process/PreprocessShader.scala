package res.process

import res.intermediate._
import res.intermediate.ProcessedShader._

import scala.collection.mutable.ArrayBuffer

object PreprocessShader {

  val PragmaImport = """^\s*#pragma\s+import\s*"([^"]*)".*$""".r

  def preprocessShader(shader: Shader, config: Config.Res.Shader): ProcessedShader = {
    val result = new ProcessedShader()
    val lines = shader.source.lines
    var chunkFirstLine = 0
    val chunkLines = ArrayBuffer[String]()

    def resolveImportFilename(filename: String): String = {
      var base = config.importPath.replace('\\', '/')
      if (base.nonEmpty && !base.endsWith("/")) base += '/'
      s"$base$filename.s2sh"
    }

    def flushChunk(): Unit = {
      if (chunkLines.nonEmpty && chunkLines.exists(_.trim.nonEmpty)) {
        val chunk = new ChunkSource(chunkLines.mkString("\n"), chunkFirstLine)
        result.chunks += chunk
      }
      chunkLines.clear()
    }

    for ((line, index) <- lines.zipWithIndex) {
      line match {
        case PragmaImport(filename) =>
          flushChunk()
          result.chunks += new ChunkImport(resolveImportFilename(filename))
        case normalLine =>
          if (chunkLines.isEmpty) chunkFirstLine = index + 1
          chunkLines += normalLine
      }
    }

    flushChunk()
    result
  }

}
