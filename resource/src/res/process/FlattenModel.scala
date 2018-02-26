package res.process

import res.intermediate._
import res.intermediate.Model._
import res.intermediate.FlatModel._

import scala.collection.mutable

/**
  * Flatten a hierarchical model node structure into a linear array of nodes
  * sorted in a way that the parent nodes always come first.
  */
object FlattenModel {

  def flattenModel(model: Model, meshMap: Map[String, (String, Material)], animMap: Map[String, String]): FlatModel = {
    val flat = new FlatModel()

    val materialMap = mutable.HashMap[Material, Int]()

    def visitNode(node: ModelNode, parentIndex: Int): Unit = {
      val nodeIndex = flat.nodes.length
      val flatNode = new FlatNode(parentIndex, node)

      flat.nodes += flatNode

      for (mesh <- node.meshes) {
        val (res, mat) = meshMap(mesh.name)
        val materialIndex = materialMap.getOrElseUpdate(mat, {
          flat.materials += mat
          flat.materials.length - 1
        })

        val flatMesh = FlatMesh(nodeIndex, mesh, res, materialIndex)
        flat.meshes += flatMesh
      }

      for (child <- node.children)
        visitNode(child, nodeIndex)
    }

    visitNode(model.root, -1)

    flat.animations = model.animations.map(animName => FlatAnimation(animName, animMap(animName)))

    flat
  }

}
