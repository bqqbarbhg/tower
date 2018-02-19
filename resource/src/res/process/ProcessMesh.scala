package res.process

import res.intermediate._

object ProcessMesh {

  /**
    * Top-level mesh processing.
    */
  def processMesh(mesh: Mesh, config: Config.Res.Mesh): Seq[GpuMesh] = {
    MeshCleanup.normalizeBoneWeights(mesh)
    ProcessMeshBones.cullBoneWeightsWithLowInfluence(mesh, config.boneCullWeight)
    MeshCleanup.sortBoneWeights(mesh)
    ProcessMeshBones.limitBoneAmountPerVertex(mesh, config.maxBonesPerVertex)
    MeshCleanup.normalizeBoneWeights(mesh)
    val parts = ProcessMeshBones.splitMeshByBoneAmount(mesh, config.maxBonesPerMesh)

    val gpuParts = for (part <- parts) yield {
      OptimizeVertexCache.optimizeVertexPostTransformCache(part)
      OptimizeVertexCache.optimizeVertexBufferIndex(part)
      CreateGpuMesh.createGpuMesh(part)
    }

    parts.foreach(_.unload())
    gpuParts
  }

}
