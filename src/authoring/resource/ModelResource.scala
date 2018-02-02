package tower.authoring.resource

import tower.math._

import scala.collection.mutable.ArrayBuffer

case class ModelMesh(val meshResource: String)

class ModelNode(val name: String) {
  var children: ArrayBuffer[ModelNode] = new ArrayBuffer[ModelNode]()
  var parent: Option[ModelNode] = None
  var transform: Matrix4 = Matrix4.Identity
  var meshes: ArrayBuffer[ModelMesh] = new ArrayBuffer[ModelMesh]()
}

class ModelResource(name: String) extends tower.authoring.Resource(name) {

  var root: ModelNode = null
  var animationResources: ArrayBuffer[String] = new ArrayBuffer[String]()

}
