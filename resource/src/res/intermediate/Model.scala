package res.intermediate

import collection.mutable.ArrayBuffer

import core._
import Model._

object Model {
  case class ModelMesh(val name: String)

  class ModelNode(val name: String) {
    var children: ArrayBuffer[ModelNode] = new ArrayBuffer[ModelNode]()
    var parent: Option[ModelNode] = None
    var transform: Matrix43 = Matrix43.Identity
    var meshes: ArrayBuffer[ModelMesh] = new ArrayBuffer[ModelMesh]()
  }
}

class Model extends Resource {
  var root: ModelNode = null
  var animations: ArrayBuffer[String] = ArrayBuffer[String]()

  override def unload(): Unit = {
    root = null
  }
}