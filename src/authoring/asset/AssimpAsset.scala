package tower.authoring.asset

import org.lwjgl.PointerBuffer
import org.lwjgl.assimp._
import org.lwjgl.assimp.Assimp._

import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer

import tower.authoring.Asset
import tower.authoring.Resource
import tower.authoring.resource._

import tower.math._

/**
  * Autodesk .fbx format file. Contains mesh, scene-graph and animation data.
  */
class AssimpAsset(filename: String) extends Asset(filename) {

  /**
    * Helper function for converting PointerBuffers to typed arrays of structs
    */
  private def collect[T: ClassTag](buf: PointerBuffer, num: Int, ctor: Long => T): Array[T] = {
    val iter = for (i <- 0 until num) yield ctor(buf.get(i))
    iter.toArray
  }

  // Helper functions to convert Assimp types to own ones
  private def convertQuat(q: AIQuaternion) = Quaternion(q.x, q.y, q.z, q.w)
  private def convertVec3(q: AIVector3D) = Vector3(q.x, q.y, q.z)
  private def convertQuatFrame(frame: AIQuatKey) = FrameQuat(frame.mTime, convertQuat(frame.mValue))
  private def convertVec3Frame(frame: AIVectorKey) = FrameVec3(frame.mTime, convertVec3(frame.mValue))

  private val animations = new ArrayBuffer[AnimationResource]
  private val meshes = new ArrayBuffer[MeshResource]

  {
    val aScene = aiImportFile(filename, aiProcess_Triangulate | aiProcess_JoinIdenticalVertices)
    val aAnims = collect(aScene.mAnimations, aScene.mNumAnimations, AIAnimation.create)
    val aMeshes = collect(aScene.mMeshes, aScene.mNumMeshes, AIMesh.create)

    for (aAnim <- aAnims) {
      val name = aAnim.mName.dataString.split('|')(1)
      val anim = new AnimationResource(name, aAnim.mDuration)

      val aChans = collect(aAnim.mChannels, aAnim.mNumChannels, AINodeAnim.create)
      for (aChan <- aChans) {
        val rot = for (i <- 0 until aChan.mNumRotationKeys) yield convertQuatFrame(aChan.mRotationKeys.get(i))
        val pos = for (i <- 0 until aChan.mNumPositionKeys) yield convertVec3Frame(aChan.mPositionKeys.get(i))
        val siz = for (i <- 0 until aChan.mNumScalingKeys)  yield convertVec3Frame(aChan.mScalingKeys.get(i))
        val timeline = new Timeline(aChan.mNodeName.dataString, rot.toArray, pos.toArray, siz.toArray)
        anim.timelines += timeline
      }

      animations += anim
    }

    for (aMesh <- aMeshes) {
      val name = aMesh.mName.dataString
      val mesh = new MeshResource(name)
      meshes += mesh
    }

    aiReleaseImport(aScene)
  }

  def resources: Seq[Resource] = animations ++ meshes

}
