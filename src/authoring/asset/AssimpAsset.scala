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
    val flags = aiProcess_Triangulate | aiProcess_JoinIdenticalVertices | aiProcess_Debone | aiProcess_GenNormals | aiProcess_GenUVCoords | aiProcess_CalcTangentSpace
    val aScene = aiImportFile(filename, flags)
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

      val numVerts = aMesh.mNumVertices
      mesh.vertices = Array.fill(numVerts)(new Vertex)
      for (i <- 0 until numVerts) {
        mesh.vertices(i).position = convertVec3(aMesh.mVertices.get(i))

        // Normals are always available
        val z = convertVec3(aMesh.mNormals.get(i))

        // If there's no tangents generate some dummy tangent space
        val x = if (aMesh.mTangents != null) {
          convertVec3(aMesh.mTangents.get(i))
        } else if (math.abs(z dot Vector3(0.0, 0.0, 1.0)) < 0.5) {
          Vector3(0.0, 0.0, 1.0)
        } else {
          Vector3(0.0, 1.0, 0.0)
        }
        val y = if (aMesh.mBitangents != null) {
         convertVec3(aMesh.mBitangents.get(i))
        } else {
          (z cross x).normalize
        }

        val qw = math.sqrt(1 + x.x + y.y + z.z)
        val qx = (y.z - z.y) / (4.0 * qw)
        val qy = (z.x - x.z) / (4.0 * qw)
        val qz = (x.y - y.x) / (4.0 * qw)
        mesh.vertices(i).tangentSpace = Quaternion(qx, qy, qz, qw)
      }

      val uvs = aMesh.mTextureCoords(0)
      if (uvs != null) {
        for (i <- 0 until numVerts) {
          val uv = uvs.get(i)
          mesh.vertices(i).uv = Vector2(uv.x, uv.y)
        }
      }

      val aBones = collect(aMesh.mBones, aMesh.mNumBones, AIBone.create)
      for ((aBone, boneIndex) <- aBones.zipWithIndex) {
        mesh.boneNames += aBone.mName.dataString
        for (weightI <- 0 until aBone.mNumWeights) {
          val aWeight = aBone.mWeights.get(weightI)
          mesh.vertices(aWeight.mVertexId).bones += BoneWeight(boneIndex, aWeight.mWeight)
        }
      }

      val numIndices = aMesh.mNumFaces * 3
      mesh.indices = new Array[Int](numIndices)
      for (i <- 0 until aMesh.mNumFaces) {
        val aFace = aMesh.mFaces.get(i)
        mesh.indices(i * 3 + 0) = aFace.mIndices.get(0)
        mesh.indices(i * 3 + 1) = aFace.mIndices.get(1)
        mesh.indices(i * 3 + 2) = aFace.mIndices.get(2)
      }

    }

    def processNode(aNode: AINode): Unit = {
      val aChildren = collect(aNode.mChildren, aNode.mNumChildren, AINode.create)
      println(s"${aNode.mName.dataString}: ${aNode.mNumMeshes}")
      for (aChild <- aChildren)
        processNode(aChild)
    }

    processNode(aScene.mRootNode)

    aiReleaseImport(aScene)
  }

  def resources: Seq[Resource] = animations ++ meshes

}
