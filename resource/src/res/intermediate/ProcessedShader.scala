package res.intermediate

import scala.collection.mutable.ArrayBuffer

import res.intermediate.ProcessedShader._


object ProcessedShader {

  abstract class Chunk
  case class ChunkSource(source: String, baseLine: Int) extends Chunk
  case class ChunkImport(filename: String) extends Chunk

}

class ProcessedShader {
  var chunks: ArrayBuffer[Chunk] = ArrayBuffer[Chunk]()
}
