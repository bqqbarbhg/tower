package tower.authoring.resource

import tower.math._

import scala.collection.mutable.ArrayBuffer

case class Material(val textureResource: String)
case class ModelMesh(val meshResource: String, val material: Material)

class ModelNode(val name: String) {
  val children: ArrayBuffer[ModelNode] = new ArrayBuffer[ModelNode]()
  var parent: Option[ModelNode] = None
  var transform: Matrix4 = Matrix4.Identity
  val meshes: ArrayBuffer[ModelMesh] = new ArrayBuffer[ModelMesh]()
}

class ModelResource(name: String) extends tower.authoring.Resource(name) {

  var root: ModelNode = null
  val animationResources: ArrayBuffer[String] = new ArrayBuffer[String]()

}
