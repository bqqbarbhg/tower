package gfx

import java.nio.ByteBuffer

import core._
import render._
import util.BufferUtils._
import MeshPart._

object MeshPart {

  val SemanticMap = Map(
    "Posi" -> Identifier("Position"),
    "TxCo" -> Identifier("TexCoord"),
    "TnSp" -> Identifier("TangentSpace"),
    "TSpZ" -> Identifier("Normal"),
    "TSpX" -> Identifier("Tangent"),
    "TSpY" -> Identifier("Bitangent"),
    "BoIx" -> Identifier("BoneIndex"),
    "BoWg" -> Identifier("BoneWeight"),
    "Pad." -> Identifier("Padding"),
  )

}

class MeshPart(val mesh: Mesh) {

  var numVertices: Int = 0
  var numIndices: Int = 0
  var numBones: Int = 0
  var maxBonesPerVertex: Int = 0

  var skinnedPartIndex: Int = -1

  var vertexDataSize: Int = 0
  var indexDataSize: Int = 0

  // Actually `Array[Identifier]` but I don't trust Scala to do the right thing here
  var boneName: Array[Int] = null
  var boneMeshToBone: Array[Matrix43] = null

  var vertexBuffer: VertexBuffer = null
  var indexBuffer: IndexBuffer = null

  var vertexSpec: VertexSpec = null

  var uvOffsetX: Float = 0.0f
  var uvOffsetY: Float = 0.0f
  var uvScaleX: Float = 0.0f
  var uvScaleY: Float = 0.0f

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

    val uvMin = buffer.getVector2()
    val uvMax = buffer.getVector2()

    maxBonesPerVertex = buffer.getInt()

    uvOffsetX = uvMin.x.toFloat
    uvOffsetY = (1.0 - uvMin.y).toFloat
    uvScaleX = (uvMax.x - uvMin.x).toFloat
    uvScaleY = -(uvMax.y - uvMin.y).toFloat

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
      val semantic = buffer.getMagic()
      val num = buffer.getShort().toInt
      val index = buffer.getShort().toInt

      val name = SemanticMap.getOrElse(semantic, {
        throw new RuntimeException(s"Unexpected vertex semantic: $semantic")
      })

      Attrib(num, fmt, name)
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

  def unload(): Unit = {
    vertexBuffer.free()
    indexBuffer.free()
  }
}
