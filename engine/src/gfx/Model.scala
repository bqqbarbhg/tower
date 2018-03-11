package gfx

import java.nio.ByteBuffer
import org.lwjgl.system.MemoryUtil

import core._
import util.BufferUtils._
import io.content.Package

object Model {
  def load(name: Identifier): Option[Model] = {
    Package.get.get(name).map(file => {
      val model = new Model()

      val buffer = MemoryUtil.memAlloc(file.sizeInBytes.toInt)
      val stream = file.read()
      buffer.readFrom(stream)
      buffer.finish()
      model.load(buffer)
      stream.close()
      MemoryUtil.memFree(buffer)

      model
    })
  }
}

class Model {

  private var isLoaded = false

  // Nodes
  var transformToParent: Array[AffineTransform] = Array[AffineTransform]()
  var transformToRoot: Array[Matrix43] = Array[Matrix43]()
  var parentIndex: Array[Int] = Array[Int]()
  var nodeName: Array[IdentifierIx] = Array[IdentifierIx]()
  var numNodes: Int = 0

  // Meshes
  var meshParentNode: Array[Int] = Array[Int]()
  var meshName: Array[IdentifierIx] = Array[IdentifierIx]()
  var meshResource: Array[IdentifierIx] = Array[IdentifierIx]()
  var meshMaterialIndex: Array[Int] = Array[Int]()
  var meshes: Array[Mesh] = Array[Mesh]()
  var numMeshes: Int = 0

  // Animations
  var animName: Array[IdentifierIx] = Array[IdentifierIx]()
  var animResource: Array[IdentifierIx] = Array[IdentifierIx]()
  var anims: Array[Animation] = Array[Animation]()
  var numAnims: Int = 0

  // Materials
  var materials: Array[Material] = Array[Material]()
  var numMaterials: Int = 0

  // Misc
  private val animationMappingCache = new java.util.IdentityHashMap[Animation, Array[Int]]

  def loaded: Boolean = isLoaded

  def load(buffer: ByteBuffer): Unit = {

    // -- Load data

    // @Deserialize(s2md)
    buffer.verifyMagic("s2md")
    val MaxVersion = 1
    val version = buffer.getVersion(MaxVersion)

    // Model header
    this.numNodes = buffer.getInt()
    this.numMeshes = buffer.getInt()
    this.numAnims = buffer.getInt()
    this.numMaterials = buffer.getInt()

    // Nodes
    this.transformToParent = new Array[AffineTransform](this.numNodes)
    this.transformToRoot = new Array[Matrix43](this.numNodes)
    this.parentIndex = new Array[Int](this.numNodes)
    this.nodeName = new Array[IdentifierIx](this.numNodes)
    for (i <- 0 until this.numNodes) {
      this.nodeName(i) = buffer.getIdentifier().index
      this.parentIndex(i) = buffer.getInt()
      this.transformToParent(i) = buffer.getAffine()
    }

    // Meshes
    this.meshParentNode = new Array[Int](this.numMeshes)
    this.meshName = new Array[IdentifierIx](this.numMeshes)
    this.meshResource = new Array[IdentifierIx](this.numMeshes)
    this.meshMaterialIndex = new Array[Int](this.numMeshes)
    this.meshes = new Array[Mesh](this.numMeshes)
    for (i <- 0 until this.numMeshes) {
      this.meshParentNode(i) = buffer.getInt()
      this.meshName(i) = buffer.getIdentifier().index
      this.meshResource(i) = buffer.getIdentifier().index
      this.meshMaterialIndex(i) = buffer.getInt()
    }

    // Animations
    this.animName = new Array[IdentifierIx](this.numAnims)
    this.animResource = new Array[IdentifierIx](this.numAnims)
    this.anims = new Array[Animation](this.numAnims)
    for (i <- 0 until this.numAnims) {
      this.animName(i) = buffer.getIdentifier().index
      this.animResource(i) = buffer.getIdentifier().index
    }

    // Materials
    this.materials = new Array[Material](this.numMaterials)
    for (i <- 0 until this.numMaterials) {
      val mat = new Material()
      mat.albedoTexRes = buffer.getIdentifier()
      mat.normalTexRes = buffer.getIdentifier()
      this.materials(i) = mat
    }

    buffer.verifyMagic("E.md")

    // -- Compute transform to root
    this.transformToRoot(0) = Matrix43.affine(this.transformToParent(0))
    for (i <- 1 until this.numNodes) {
      val parent = this.parentIndex(i)
      this.transformToRoot(i) = Matrix43.affine(this.transformToParent(i)) * this.transformToRoot(parent)
    }

    isLoaded = true
  }

  /** Mark the model as unloaded */
  def unload(): Unit = {
    isLoaded = false
  }

  /** Load the content required by this Model */
  def loadContent(): Unit = {
    for (i <- 0 until this.numMeshes) {
      this.meshes(i) = Mesh.load(new Identifier(this.meshResource(i))).orNull
      this.meshes(i).material = this.materials(this.meshMaterialIndex(i))
    }
    for (i <- 0 until this.numAnims) {
      this.anims(i) = Animation.load(new Identifier(this.animResource(i))).orNull
    }
    for (material <- this.materials) {
      if (material.albedoTexRes != Identifier.Empty)
        material.albedoTex = Texture.load(material.albedoTexRes).getOrElse(Material.missingAlbedo)
      else
        material.albedoTex = Material.missingAlbedo

      if (material.normalTexRes != Identifier.Empty)
        material.normalTex = Texture.load(material.normalTexRes).getOrElse(Material.missingNormal)
      else
        material.normalTex = Material.missingNormal
    }
  }

  /**
    * Find an animation by name. Returns `null` on failure (for performance reasons).
    */
  def findAnimationByName(name: Identifier): Animation = {
    var index = 0
    while (index < numAnims) {
      if (name.index == animName(index)) return anims(index)
      index += 1
    }
    null
  }

  /**
    * Find a node by name. Returns -1 on failure (for performance reasons).
    */
  def findNodeByName(name: Identifier): Int = {
    var index = 0
    while (index < numNodes) {
      if (name.index == nodeName(index)) return index
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
    val mapping: Array[Int] = animation.timelines.map(tl => findNodeByName(tl.bone))
    animationMappingCache.put(animation, mapping)
    mapping
  }
}
