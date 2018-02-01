package tower.authoring.processing

import tower.authoring.resource._

object MeshProcessing {

  /**
    * Sorts the vertex bone influences by descending weight.
    */
  def sortBoneWeights(mesh: MeshResource): Unit = {
    for (vert <- mesh.vertices) {
      vert.bones = vert.bones.sortBy(-_.weight)
    }
  }

  /**
    * Normalize all the bone weights so they sum to one.
    */
  def normalizeBoneWeights(mesh: MeshResource): Unit = {
    for (vert <- mesh.vertices) {
      val sum = vert.bones.map(_.weight).sum
      vert.bones.map(b => b.copy(weight = b.weight / sum))
    }
  }

}
