package tower.authoring.processing

import tower.authoring.resource._

object MeshPreprocessing {

  /**
    * Sorts the vertex bone influences by descending weight.
    */
  def sortBoneWeights(mesh: MeshResource): Unit = {
    for (vert <- mesh.vertices) {
      vert.bones = vert.bones.sortBy(-_.weight)
    }
  }

}
