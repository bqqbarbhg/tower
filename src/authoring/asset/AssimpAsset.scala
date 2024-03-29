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
class AssimpAsset(filename: String, baseName: String) extends Asset(filename, baseName) {

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
  private def convertQuatFrame(frame: AIQuatKey, timeScale: Double) = FrameQuat(frame.mTime * timeScale, convertQuat(frame.mValue))
  private def convertVec3Frame(frame: AIVectorKey, timeScale: Double) = FrameVec3(frame.mTime * timeScale, convertVec3(frame.mValue))
  private def convertMat4(m: AIMatrix4x4) = {
    val r = new Matrix4()
    r.m11 = m.a1; r.m12 = m.a2; r.m13 = m.a3; r.m14 = m.a4
    r.m21 = m.b1; r.m22 = m.b2; r.m23 = m.b3; r.m24 = m.b4
    r.m31 = m.c1; r.m32 = m.c2; r.m33 = m.c3; r.m34 = m.c4
    r.m41 = m.d1; r.m42 = m.d2; r.m43 = m.d3; r.m44 = m.d4

    // Let's just make sure there's no surprises in these matrices!
    assert(math.abs(r.m41 - 0.0) < 0.001, r.m41)
    assert(math.abs(r.m42 - 0.0) < 0.001, r.m42)
    assert(math.abs(r.m43 - 0.0) < 0.001, r.m43)
    assert(math.abs(r.m44 - 1.0) < 0.001, r.m44)

    r
  }

  private val animations = new ArrayBuffer[AnimationResource]
  private val meshes = new ArrayBuffer[MeshResource]
  private var model: ModelResource = new ModelResource(s"$baseName.s2md")

  {
    val flags = aiProcess_Triangulate | aiProcess_JoinIdenticalVertices | aiProcess_Debone | aiProcess_GenNormals | aiProcess_GenUVCoords | aiProcess_CalcTangentSpace
    val aScene = aiImportFile(filename, flags)
    val aAnims = collect(aScene.mAnimations, aScene.mNumAnimations, AIAnimation.create)
    val aMeshes = collect(aScene.mMeshes, aScene.mNumMeshes, AIMesh.create)

    for (aAnim <- aAnims) {
      val name = aAnim.mName.dataString.split('|')(1)
      val anim = new AnimationResource(s"$baseName.$name.s2an", aAnim.mDuration / aAnim.mTicksPerSecond)

      anim.ticksPerSecond = aAnim.mTicksPerSecond
      val timeScale = 1.0 / anim.ticksPerSecond

      val aChans = collect(aAnim.mChannels, aAnim.mNumChannels, AINodeAnim.create)
      for (aChan <- aChans) {
        val rot = for (i <- 0 until aChan.mNumRotationKeys) yield convertQuatFrame(aChan.mRotationKeys.get(i), timeScale)
        val pos = for (i <- 0 until aChan.mNumPositionKeys) yield convertVec3Frame(aChan.mPositionKeys.get(i), timeScale)
        val siz = for (i <- 0 until aChan.mNumScalingKeys)  yield convertVec3Frame(aChan.mScalingKeys.get(i), timeScale)
        val timeline = new Timeline(aChan.mNodeName.dataString, rot.toArray, pos.toArray, siz.toArray)
        anim.timelines += timeline
      }

      animations += anim
    }

    for (aMesh <- aMeshes) {
      val name = aMesh.mName.dataString
      val mesh = new MeshResource(s"$baseName.$name.s2ms")
      meshes += mesh

      val numVerts = aMesh.mNumVertices
      mesh.vertices = Array.fill(numVerts)(new Vertex)
      for (i <- 0 until numVerts) {
        mesh.vertices(i).position = convertVec3(aMesh.mVertices.get(i))

        // Normals are always available
        val z = convertVec3(aMesh.mNormals.get(i)).normalize

        // If there's no tangents generate some dummy tangent space
        val (x, y) = if (aMesh.mTangents != null && aMesh.mBitangents != null) {
          val ref = convertVec3(aMesh.mTangents.get(i))
          val yref = (z cross ref).normalize
          val x = (z cross yref).normalize
          val y = (z cross x).normalize
          (x, y)
        } else {
          val ref = if (math.abs(z dot Vector3(0.0, 0.0, 1.0)) < 0.5) {
            Vector3(0.0, 0.0, 1.0)
          } else {
            Vector3(0.0, 1.0, 0.0)
          }

          val yref = (z cross ref).normalize
          val x = (z cross yref).normalize
          val y = (z cross x).normalize
          (x, y)
        }

        assert(math.abs(x.length - 1.0) < 0.001)
        assert(math.abs(y.length - 1.0) < 0.001)
        assert(math.abs(z.length - 1.0) < 0.001)
        assert(math.abs(x dot y) < 0.001, (x dot y))
        assert(math.abs(y dot z) < 0.001, (y dot z))
        assert(math.abs(z dot x) < 0.001, (z dot x))
        val quat = Quaternion.fromAxes(x, y, z)
        assert(math.abs(quat.length - 1.0) < 0.001, s"Length should be 1, was ${quat.length} ($x $y $z)")
        mesh.vertices(i).tangentSpace = quat.normalize
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
        val bone = new MeshBone
        bone.name = aBone.mName.dataString
        bone.meshToBone = convertMat4(aBone.mOffsetMatrix)
        mesh.bones += bone
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

    def processNode(aNode: AINode): ModelNode = {
      val aChildren = collect(aNode.mChildren, aNode.mNumChildren, AINode.create)

      val node = new ModelNode(aNode.mName.dataString)
      node.transform = convertMat4(aNode.mTransformation)

      for (i <- 0 until aNode.mNumMeshes) {
        val meshIndex = aNode.mMeshes.get(i)
        node.meshes += ModelMesh(meshes(i).name)
      }

      node.children = aChildren.map(processNode).to[ArrayBuffer]
      node
    }

    model.root = processNode(aScene.mRootNode)
    model.animationResources = animations.map(_.name)

    aiReleaseImport(aScene)
  }

  def resources: Seq[Resource] = animations ++ meshes :+ model

}
