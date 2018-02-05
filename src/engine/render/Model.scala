package tower.engine.render

import tower.math._
import java.io.InputStream
import java.nio.ByteBuffer

import tower.util.Serialization.ByteBufferExtension
import tower.util.Identifier

class Model {

  var name: Identifier = Identifier.Empty

  // Nodes
  var transformToParent: Array[Matrix43] = Array[Matrix43]()
  var transformToRoot: Array[Matrix43] = Array[Matrix43]()
  var parentIndex: Array[Int] = Array[Int]()
  var nodeName: Array[Identifier] = Array[Identifier]()
  var numNodes: Int = 0

  // Meshes
  var meshParentNode: Array[Int] = Array[Int]()
  var meshResource: Array[Identifier] = Array[Identifier]()
  var numMeshes: Int = 0

  // Animations
  var animResource: Array[Identifier] = Array[Identifier]()
  var numAnims: Int = 0

  // Misc
  private val animationMappingCache = new java.util.IdentityHashMap[Animation, Array[Int]]

  def load(buffer: ByteBuffer): Unit = {

    // -- Load data

    // @Serialize(s2md)
    buffer.verifyMagic("s2md")
    val MaxVersion = 1
    val version = buffer.getVersion(MaxVersion)

    // Model header
    this.name = buffer.getIdentifier()
    this.numNodes = buffer.getInt()
    this.numMeshes = buffer.getInt()
    this.numAnims = buffer.getInt()

    // Nodes
    this.transformToParent = new Array[Matrix43](this.numNodes)
    this.transformToRoot = new Array[Matrix43](this.numNodes)
    this.parentIndex = new Array[Int](this.numNodes)
    this.nodeName = new Array[Identifier](this.numNodes)
    for (i <- 0 until this.numNodes) {
      this.nodeName(i) = buffer.getIdentifier()
      this.transformToParent(i) = buffer.getMatrix43()
      this.parentIndex(i) = buffer.getInt()
    }

    // Meshes
    this.meshParentNode = new Array[Int](this.numNodes)
    this.meshResource = new Array[Identifier](this.numNodes)
    for (i <- 0 until this.numMeshes) {
      this.meshParentNode(i) = buffer.getInt()
      this.meshResource(i) = buffer.getIdentifier()
    }

    // Animations
    this.animResource = new Array[Identifier](this.numAnims)
    for (i <- 0 until this.numAnims) {
      this.animResource(i) = buffer.getIdentifier()
    }

    buffer.verifyMagic("E.md")

    // -- Compute transform to root
    this.transformToRoot(0) = Matrix43.Identity
    for (i <- 1 until this.numNodes) {
      val parent = this.parentIndex(i)
      this.transformToRoot(i) = this.transformToParent(i) * this.transformToRoot(parent)
    }
  }

  /**
    * Find a node by name. Returns -1 on failure (for performance reasons).
    */
  def findNodeByName(name: Identifier): Int = {
    var index = 0
    while (index < numNodes) {
      if (name == nodeName(index)) return index
      index += 1
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
