package res.intermediate

import java.io.FileOutputStream
import java.nio.ByteBuffer
import collection.mutable.ArrayBuffer

import core._
import res.intermediate.Model._
import res.intermediate.FlatModel._

object FlatModel {
  case class FlatNode(parent: Int, node: ModelNode)
  case class FlatMesh(parent: Int, mesh: ModelMesh)
}

class FlatModel extends Resource {
  var nodes = new ArrayBuffer[FlatNode]()
  var meshes = new ArrayBuffer[FlatMesh]()

  override def unload(): Unit = { }
}

