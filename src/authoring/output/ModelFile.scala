package tower.authoring.output

import java.io.FileOutputStream
import java.nio.ByteBuffer

import tower.authoring.resource._
import tower.util.Serialization.ByteBufferExtension
import tower.math._
import tower.util.SharedByteBuffer

import scala.collection.mutable.ArrayBuffer

object ModelFile {

  private case class FlatNode(parent: Int, node: ModelNode)
  private case class FlatMesh(parent: Int, mesh: ModelMesh)

  def save(filename: String, model: ModelResource): Unit = {

    val nodes = new ArrayBuffer[FlatNode]()
    val meshes = new ArrayBuffer[FlatMesh]()

    def visitNode(node: ModelNode, parentIndex: Int): Unit = {
      val nodeIndex = nodes.length
      val flatNode = new FlatNode(parentIndex, node)

      nodes += flatNode

      for (mesh <- node.meshes) {
        val flatMesh = FlatMesh(nodeIndex, mesh)
        meshes += flatMesh
      }

      for (child <- node.children)
        visitNode(child, nodeIndex)
    }

    visitNode(model.root, -1)

    // @Serialize(s2md)

    val buffer = SharedByteBuffer.acquire()

    val Version = 1
    buffer.putMagic("s2md")
    buffer.putVersion(Version)

    buffer.putIdentifier(model.name)
    buffer.putInt(nodes.length)
    buffer.putInt(meshes.length)
    buffer.putInt(model.animationResources.length)

    for (node <- nodes) {
      buffer.putIdentifier(node.node.name)
      buffer.putMatrix43(node.node.transform)
      buffer.putInt(node.parent)
    }

    for (mesh <- meshes) {
      buffer.putInt(mesh.parent)
      buffer.putIdentifier(mesh.mesh.meshResource)
    }

    for (anim <- model.animationResources) {
      buffer.putIdentifier(anim)
    }

    buffer.putMagic("E.md")

    val output = new FileOutputStream(filename)
    buffer.writeTo(output)
    output.close()

    SharedByteBuffer.release(buffer)
  }

}
