package authoring.asset

import org.lwjgl.PointerBuffer
import org.lwjgl.assimp._
import org.lwjgl.assimp.Assimp._

import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer

import authoring.Asset
import authoring.Resource
import authoring.resource._

import util.math._

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
  private def convertQuat(q: AIQuaternion): Quaternion = Quaternion(q.x, q.y, q.z, q.w)
  private def convertRotFrame(frame: AIQuatKey): FrameRot = FrameRot(frame.mTime, convertQuat(frame.mValue))

  private val animations = new ArrayBuffer[Animation]
  private val meshes = new ArrayBuffer[Mesh]

  {
    val aScene = aiImportFile(filename, aiProcess_Triangulate | aiProcess_JoinIdenticalVertices)
    val aAnims = collect(aScene.mAnimations, aScene.mNumAnimations, AIAnimation.create)
    val aMeshes = collect(aScene.mMeshes, aScene.mNumMeshes, AIMesh.create)

    for (aAnim <- aAnims) {
      val name = aAnim.mName.dataString
      val anim = new Animation(name)

      val aChans = collect(aAnim.mChannels, aAnim.mNumChannels, AINodeAnim.create)
      for (aChan <- aChans) {
        val rot = for (i <- 0 until aChan.mNumRotationKeys) yield convertRotFrame(aChan.mRotationKeys.get(i))
        val timeline = new Timeline(aChan.mNodeName.dataString, rot.toArray)
        anim.timelines += timeline
      }

      animations += anim
    }

    for (aMesh <- aMeshes) {
      val name = aMesh.mName.dataString
      val mesh = new Mesh(name)
      meshes += mesh
    }

    aiReleaseImport(aScene)
  }

  def resources: Seq[Resource] = animations ++ meshes

}
