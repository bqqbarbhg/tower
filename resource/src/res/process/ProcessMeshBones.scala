package res.process

import collection.immutable
import collection.mutable
import math.Ordering.Implicits._
import collection.mutable.ArrayBuffer

import res.intermediate._
import res.intermediate.Mesh._

object ProcessMeshBones {
  /**
    * Forces a maximum limit of bone weights influencing one vertex. Assumes that bones have been
    * pre-sorted into preferred importance order (for example with `MeshPreprocessing.sortBoneWeights`).
    */
  def limitBoneAmountPerVertex(mesh: Mesh, maxBonesPerVertex: Int): Unit = {
    for (vert <- mesh.vertices) {
      vert.bones = vert.bones.take(maxBonesPerVertex)
    }
  }

  /**
    * Removes bone weights that have influence lower than some minimum value. This should be done
    * before splitting the mesh into chunks as it can improve the chunk efficiency.
    */
  def cullBoneWeightsWithLowInfluence(mesh: Mesh, minimumInfluence: Double): Unit = {
    for (vert <- mesh.vertices) {
      vert.bones = vert.bones.filter(_.weight >= minimumInfluence)
    }
  }

  private type BoneSet = immutable.Set[Int]
  private type IndexList = immutable.Vector[Int]
  private type BoneMap = mutable.HashMap[BoneSet, IndexList]

  /**
    * Split the mesh in a way that each submesh has `<= maxBonesPerMesh` bones.
    */
  def splitMeshByBoneAmount(mesh: Mesh, maxBonesPerMesh: Int): Seq[Mesh] = {

    // Fast case: Already valid mesh
    if (mesh.bones.length <= maxBonesPerMesh) {
      return Array(mesh)
    }

    val boneMap = (new BoneMap).withDefaultValue(immutable.Vector[Int]())

    for (base <- 0 until mesh.indices.length by 3) {
      val indices = for (i <- 0 until 3) yield mesh.indices(base + i)
      val bonesPerVertex = for (i <- indices) yield mesh.vertices(i).bones.map(_.index).toSet
      val bones = bonesPerVertex.reduce(_ ++ _)
      if (bones.size > maxBonesPerMesh) {
        throw new RuntimeException(s"Triangle bones than max bones per mesh: ${bones.size} > $maxBonesPerMesh")
      }
      boneMap(bones) ++= indices
    }

    assert(boneMap.map(_._2.length).sum == mesh.indices.length)

    val indexGroups = mutable.ArrayBuffer[(BoneSet, IndexList)]()

    while (boneMap.nonEmpty) {
      // Pick the biggest set and try to expand it
      val (bones, indices) = boneMap.maxBy(_._2.length)
      boneMap.remove(bones)

      // Filter all the sets that can be added to this without breaking the budget
      val options = boneMap.filter(pair => (pair._1 ++ bones).size <= maxBonesPerMesh)
      if (options.size == 0) {
        // Nothing can be added anymore -> shift this set out from the working group
        indexGroups.append((bones, indices))
      } else {
        // Select the "best" addition from the list of possibilities.
        // The best is defined by heuristic `newIndices / newBones` which has the property of
        // preferring additions that don't add many bones. Also define that if `newBones` goes
        // to zero the heuristic goes to infinity -> prefer adding "free" sets
        val (bones2, indices2) = options.maxBy(pair => {
          val newBonesToSet = (pair._1 &~ bones).size
          val newIndicesToSet = pair._2.length
          // Technically the division would give the same result but let's do this explicitly
          if (newBonesToSet == 0) {
            Double.PositiveInfinity
          } else {
            newIndicesToSet.toDouble / math.pow(newBonesToSet.toDouble, 2.0)
          }
        })
        boneMap.remove(bones2)

        // Combine the sets and re-insert
        boneMap(bones ++ bones2) ++= indices ++ indices2
      }
    }

    assert(indexGroups.map(_._2.length).sum == mesh.indices.length)

    var meshPartIndex = 0
    // Remap the group vertex/bone indices
    val parts: ArrayBuffer[Mesh] = for ((bones, indices) <- indexGroups) yield {
      val meshPart = new Mesh(mesh.name + "." + meshPartIndex)
      val boneRemap = bones.toSeq.sorted.zipWithIndex.toMap
      val vertexRemap = indices.toSet.toSeq.sorted.zipWithIndex.toMap

      meshPart.vertices = new Array[Vertex](vertexRemap.size)
      meshPart.bones = mutable.ArrayBuffer.fill(boneRemap.size)(null)
      meshPart.indices = indices.map(vertexRemap).toArray

      for ((src, dst) <- boneRemap) {
        meshPart.bones(dst) = mesh.bones(src)
      }

      for ((src, dst) <- vertexRemap) {
        val vert: Vertex = mesh.vertices(src).copy
        vert.bones = vert.bones.map(b => b.copy(index = boneRemap(b.index)))
        meshPart.vertices(dst) = vert
      }

      meshPartIndex += 1
      meshPart
    }

    parts
  }


}

