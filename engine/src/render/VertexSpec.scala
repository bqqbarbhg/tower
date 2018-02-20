package render

import VertexSpec._

object VertexSpec {

  sealed abstract class Semantic(val nameInShader: String)
  object Semantic {
    case object Position extends Semantic("Position")
    case object TexCoord extends Semantic("TexCoord")
    case object TangentSpace extends Semantic("TangentSpace")
    case object BoneIndex extends Semantic("BoneIndex")
    case object BoneWeight extends Semantic("BoneWeight")
    case object Padding extends Semantic("")
  }

  sealed abstract class DataFmt(val sizeInBytes: Int)
  object DataFmt {
    case object F32 extends DataFmt(4)
    case object SN8 extends DataFmt(1)
    case object UN8 extends DataFmt(1)
    case object UI8 extends DataFmt(1)
    case object SN16 extends DataFmt(2)
    case object UN16 extends DataFmt(2)
    case object PAD extends DataFmt(1)
  }

  case class Attrib(num: Int, fmt: DataFmt, semantic: Semantic, index: Int = 0) {
    def sizeInBytes: Int = fmt.sizeInBytes * num
  }
}

case class VertexSpec(attribs: Vector[Attrib]) {
  val sizeInBytes = attribs.map(a => a.sizeInBytes).sum
}
