package res.intermediate

import scala.collection.mutable.ArrayBuffer

import res.intermediate.ProcessedShader._


object ProcessedShader {

  abstract class Chunk
  case class ChunkSource(source: String, baseLine: Int) extends Chunk
  case class ChunkImport(filename: String) extends Chunk

  case class Extension(name: String, behavior: String)

}

class ProcessedShader {
  var chunks: ArrayBuffer[Chunk] = ArrayBuffer[Chunk]()
  var extensions: ArrayBuffer[Extension] = ArrayBuffer[Extension]()
}
