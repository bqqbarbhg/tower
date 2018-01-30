package tower.authoring.resource

import scala.collection.mutable.ArrayBuffer

import tower.Identifier
import tower.math._

case class BoneWeight(val index: Int, val weight: Double)

class Vertex {
  var position: Vector3 = Vector3.Zero
  var uv: Vector2 = Vector2.Zero
  var bones: ArrayBuffer[BoneWeight] = ArrayBuffer[BoneWeight]()
  var tangentSpace: Quaternion = Quaternion.Identity
}

class MeshResource(name: String) extends tower.authoring.Resource(name) {

  val boneNames = ArrayBuffer[String]()
  var vertices = Array[Vertex]()
  var indices = Array[Int]()

}
