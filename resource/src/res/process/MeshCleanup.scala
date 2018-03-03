package res.process

import res.intermediate._

object MeshCleanup {

  /**
    * Sorts the vertex bone influences by descending weight.
    */
  def sortBoneWeights(mesh: Mesh): Unit = {
    for (vert <- mesh.vertices) {
      vert.bones = vert.bones.sortBy(-_.weight)
    }
  }

  /**
    * Normalize all the bone weights so they sum to one.
    */
  def normalizeBoneWeights(mesh: Mesh): Unit = {
    for (vert <- mesh.vertices) {
      val sum = vert.bones.map(_.weight).sum
      vert.bones = vert.bones.map(b => b.copy(weight = b.weight / sum))
    }
  }

}
