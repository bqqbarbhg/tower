package tower.engine.render

import tower.Identifier
import tower.math._

class Model {

  // Nodes
  var transformToParent: Array[Matrix4] = Array[Matrix4]()
  var transformToRoot: Array[Matrix4] = Array[Matrix4]()
  var parentIndex: Array[Int] = Array[Int]()
  var nodeName: Array[Identifier] = Array[Identifier]()
  var numNodes: Int = 0

  // Meshes
  var parentNode: Array[Int] = Array[Int]()
  var numMeshes: Int = 0

  // Misc
  private val animationMappingCache = new java.util.IdentityHashMap[Animation, Array[Int]]

  /**
    * Find a node by name. Returns -1 on failure (for performance reasons).
    */
  def findNodeByName(name: Identifier): Int = {
    var index = 0
    while (index < numNodes) {
      if (name == nodeName(index)) return index
    }
    -1
  }

  /**
    * Resolves a mapping of animation timelines to nodes on this model.
    * @return Array of node indices corresponding to the timelines of the animation.
    */
  def resolveAnimationTimelineNodeIndices(animation: Animation): Array[Int] = {
    val cached = animationMappingCache.get(animation)
    if (cached != null) return cached
    val mapping: Array[Int] = animation.timelines.map(tl => {
      val index = findNodeByName(tl.bone)
      assert(index >= 0)
      index
    })
    animationMappingCache.put(animation, mapping)
    mapping
  }

}
