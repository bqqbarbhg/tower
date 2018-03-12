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
  var nodeTransform = model.transformToParent.take(model.numNonAuxilaryNodes)

  /**
    * Spares local transformation matrices for nodes. Will be applied before
    * parent-to-node transform.
    *
    * Optimization: Uses `null` for identity transforms.
    * Optimization: Reset to `null` after call to `updateMatrices()`
    */
  var nodeLocalMatrix = Array.fill[Matrix43](model.numNonAuxilaryNodes)(null)

  /** Unsafe world transform matrices for the nodes, instance contents change on
    * call to `updateMatrices()` */
  val nodeWorldTransform = Array.fill(model.numNonAuxilaryNodes)(Matrix43.unsafeIdentity)

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

      if (nodeLocalMatrix(0) != null) {
        nodeWorldTransform(0).unsafeMulRight(nodeLocalMatrix(0))
        nodeLocalMatrix(0) = null
      }
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

      if (nodeLocalMatrix(ix) != null) {
        nodeWorldTransform(ix).unsafeMulRight(nodeLocalMatrix(ix))
        nodeLocalMatrix(ix) = null
      }

      ix += 1
    }
  }

}
