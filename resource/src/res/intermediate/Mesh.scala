package res.intermediate

import collection.mutable.ArrayBuffer

import core._
import Mesh._

object Mesh {
  case class BoneWeight(val index: Int, val weight: Double)

  class Vertex {
    var position: Vector3 = Vector3.Zero
    var uv: Vector2 = Vector2.Zero
    var bones: ArrayBuffer[BoneWeight] = ArrayBuffer[BoneWeight]()
    var normal: Vector3 = Vector3.Zero
    var tangent: Vector3 = Vector3.Zero
    var bitangent: Vector3 = Vector3.Zero

    def copy: Vertex = {
      val vert = new Vertex
      vert.position = position.copy()
      vert.uv = uv.copy()
      vert.bones = bones.map(_.copy())
      vert.normal = normal.copy()
      vert.tangent = tangent.copy()
      vert.bitangent = bitangent.copy()
      vert
    }
  }

  class MeshBone {
    var name: String = ""
    var meshToBone: Matrix43 = Matrix43.Identity
  }
}

class Mesh(val name: String) extends Resource {

  var bones = ArrayBuffer[MeshBone]()
  var vertices = Array[Vertex]()
  var indices = Array[Int]()

  var textureName: String = ""
  var material: Material = null

  override def unload(): Unit = {
    bones = ArrayBuffer[MeshBone]()
    vertices = Array[Vertex]()
    indices = Array[Int]()
  }
}