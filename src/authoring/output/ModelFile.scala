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

    buffer.putMagic("E.md")

    SharedByteBuffer.release(buffer)
  }

}
