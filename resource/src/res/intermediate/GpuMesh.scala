package res.intermediate

import java.nio.ByteBuffer
import org.lwjgl.system.MemoryUtil

import core._
import GpuMesh._

object GpuMesh {

  sealed abstract class Semantic(val magic: String)
  object Semantic {
    case object Position extends Semantic("Posi")
    case object TexCoord extends Semantic("TxCo")
    case object TangentSpace extends Semantic("TnSp")
    case object BoneIndex extends Semantic("BoIx")
    case object BoneWeight extends Semantic("BoWg")
    case object Padding extends Semantic("Pad.")
  }

  sealed abstract class DataFmt(val magic: String, val sizeInBytes: Int)
  object DataFmt {
    case object F32 extends DataFmt("F32.", 4)
    case object SN8 extends DataFmt("SN8.", 1)
    case object UN8 extends DataFmt("UN8.", 1)
    case object UI8 extends DataFmt("UI8.", 1)
    case object SN16 extends DataFmt("SN16", 2)
    case object UN16 extends DataFmt("UN16", 2)
    case object PAD extends DataFmt("PAD.", 1)
  }

  case class Attrib(num: Int, fmt: DataFmt, semantic: Semantic, index: Int = 0)

  case class VertexSpec(attribs: Vector[Attrib]) {
    val sizeInBytes = attribs.map(a => a.num * a.fmt.sizeInBytes).sum
  }

}

class GpuMesh(val name: String) extends Resource {
  var bones: Array[Mesh.MeshBone] = Array[Mesh.MeshBone]()
  var numVertices: Int = 0
  var numIndices: Int = 0

  var uvMin: Vector2 = Vector2.Zero
  var uvMax: Vector2 = Vector2.Zero

  var vertexSpec: VertexSpec = null

  def hasBones: Boolean = bones.nonEmpty

  /** Vertex data, allocated with BufferUtils.memAlloc */
  var vertexData: ByteBuffer = null
  /** Index data, allocated with BufferUtils.memAlloc */
  var indexData: ByteBuffer = null

  override def unload(): Unit = {
    MemoryUtil.memFree(vertexData)
    MemoryUtil.memFree(indexData)
  }
}

