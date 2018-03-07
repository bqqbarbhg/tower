package gfx

import core._

class ModelState(val model: Model) {

  /**
    * Root transform of the model.
    */
  var worldTransform = Matrix43.Identity

  /**
    * Transform for each of the nodes in the model.
    * Will be reset after a call to updateMatrices()
    */
  var nodeTransform = model.transformToParent.clone()

  /** Unsafe world transform matrices for the nodes, instance contents change on
    * call to `updateMatrices()` */
  val nodeWorldTransform = Array.fill(model.numNodes)(Matrix43.unsafeIdentity)

  private var tmpMatrix = new Matrix43.Unsafe()

  /**
    * Calculate the world transform matrices for each node.
    */
  def updateMatrices(): Unit = {
    // Fold `worldTransform` to root node.
    {
      val affine = nodeTransform(0)
      Matrix43.unsafeAffine(tmpMatrix, affine)
      nodeWorldTransform(0).unsafeMul(worldTransform, tmpMatrix)
      nodeTransform(0) = model.transformToParent(0)
    }

    // Propagate transform to the rest of the nodes
    val numTransform = nodeWorldTransform.length
    var ix = 1
    while (ix < numTransform) {
      val parentIx = model.parentIndex(ix)

      val affine = nodeTransform(ix)
      Matrix43.unsafeAffine(tmpMatrix, affine)

      nodeWorldTransform(ix).unsafeMul(nodeWorldTransform(parentIx), tmpMatrix)
      nodeTransform(ix) = model.transformToParent(ix)
      ix += 1
    }
  }

}