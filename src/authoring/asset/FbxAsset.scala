package authoring.asset

import org.lwjgl.PointerBuffer
import org.lwjgl.assimp._
import org.lwjgl.assimp.Assimp._

import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer

import authoring.Asset
import authoring.Resource
import authoring.resource._

/**
  * Autodesk .fbx format file. Contains mesh, scene-graph and animation data.
  */
class FbxAsset(filename: String) extends Asset(filename) {

  /**
    * Helper function for converting PointerBuffers to typed arrays of structs
    */
  private def collect[T: ClassTag](buf: PointerBuffer, num: Int, ctor: Long => T): Array[T] = {
    val iter = for (i <- 0 until num) yield ctor(buf.get(i))
    iter.toArray
  }

  private val animations = new ArrayBuffer[Animation]
  private val meshes = new ArrayBuffer[Mesh]

  {
    val scene = aiImportFile(filename, aiProcess_Triangulate | aiProcess_JoinIdenticalVertices)
    val sceneAnimations = collect(scene.mAnimations, scene.mNumAnimations, AIAnimation.create)
    val sceneMeshes = collect(scene.mMeshes, scene.mNumMeshes, AIMesh.create)

    for (animation <- sceneAnimations) {
      val name = animation.mName.dataString
      val anim = new Animation(name)
      animations += anim
    }

    for (sceneMesh <- sceneMeshes) {
      val name = sceneMesh.mName.dataString
      val mesh = new Mesh(name)
      meshes += mesh
    }

    aiReleaseImport(scene)
  }

  def resources: Seq[Resource] = animations ++ meshes

}
