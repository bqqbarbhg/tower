package gfx

import java.nio.ByteBuffer

import core._
import render._
import util.BufferUtils._

class MeshPart {

  var numVertices: Int = 0
  var numIndices: Int = 0
  var numBones: Int = 0

  var vertexDataSize: Int = 0
  var indexDataSize: Int = 0

  // Actually `Array[Identifier]` but I don't trust Scala to do the right thing here
  var boneName: Array[Int] = null
  var boneMeshToBone: Array[Matrix43] = null

  var vertexBuffer: VertexBuffer = null
  var indexBuffer: IndexBuffer = null

  var vertexSpec: VertexSpec = null

  def load(buffer: ByteBuffer): Unit = {
    // @Deserialize(s2mp)

    val MaxVersion = 1
    buffer.verifyMagic("s2mp")
    val version = buffer.getVersion(MaxVersion)

    numVertices = buffer.getInt()
    numIndices = buffer.getInt()
    numBones = buffer.getInt()

    vertexDataSize = buffer.getInt()
    indexDataSize = buffer.getInt()

    val numAttribs = buffer.getInt()

    boneName = new Array[Int](numBones)
    boneMeshToBone = new Array[Matrix43](numBones)

    for (boneI <- 0 until numBones) {
      boneName(boneI) = buffer.getIdentifier().index
      boneMeshToBone(boneI) = buffer.getMatrix43()
    }

    val attribs = Vector.tabulate(numAttribs)(ix => {
      import VertexSpec._

      val fmt = buffer.getMagic() match {
        case "F32." => DataFmt.F32
        case "SN8." => DataFmt.SN8
        case "UN8." => DataFmt.UN8
        case "UI8." => DataFmt.UI8
        case "SN16" => DataFmt.SN16
        case "UN16" => DataFmt.UN16
        case "PAD." => DataFmt.PAD
      }
      val sem = buffer.getMagic() match {
        case "Posi" => Semantic.Position
        case "TxCo" => Semantic.TexCoord
        case "TnSp" => Semantic.TangentSpace
        case "BoIx" => Semantic.BoneIndex
        case "BoWg" => Semantic.BoneWeight
        case "Pad." => Semantic.Padding
      }
      val num = buffer.getShort().toInt
      val index = buffer.getShort().toInt

      Attrib(num, fmt, sem, index)
    })

    vertexSpec = VertexSpec(attribs)

    val vertexData = buffer.getBuffer(vertexDataSize)
    vertexBuffer = VertexBuffer.createStatic(vertexSpec, vertexData)

    val indexData = buffer.getBuffer(indexDataSize)
    indexBuffer = IndexBuffer.createStatic(indexData)

    buffer.verifyMagic("E.mp")
  }

  def draw(): Unit = {
    Renderer.get.drawElements(numIndices, indexBuffer, vertexBuffer)
  }

}
