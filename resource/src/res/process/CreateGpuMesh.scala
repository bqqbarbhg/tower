package res.process

import math.{max, min}
import org.lwjgl.system.MemoryUtil

import core._
import res.intermediate._
import res.intermediate.Mesh._
import res.intermediate.GpuMesh._

object CreateGpuMesh {

  /** Convert the intermediate `Mesh` to a GPU-friendly format. */
  def createGpuMesh(mesh: Mesh): GpuMesh = {
    val gpuMesh = new GpuMesh(mesh.name)

    val uv = mesh.vertices.map(_.uv)
    gpuMesh.uvMin = uv.reduce((a, b) => Vector2(min(a.x, b.x), min(a.y, b.y)))
    gpuMesh.uvMax = uv.reduce((a, b) => Vector2(max(a.x, b.x), max(a.y, b.y)))
    gpuMesh.bones = mesh.bones.toArray

    gpuMesh.maxBonesPerVertex = mesh.vertices.map(_.bones.length).max

    gpuMesh.numIndices = mesh.indices.length
    gpuMesh.numVertices = mesh.vertices.length

    var attribs = Vector(
      Attrib(3, DataFmt.F32,  Semantic.Position),
      Attrib(2, DataFmt.UN16, Semantic.TexCoord),
      Attrib(4, DataFmt.SN8,  Semantic.Normal),
      Attrib(4, DataFmt.SN8,  Semantic.Tangent),
      Attrib(4, DataFmt.SN8,  Semantic.Bitangent),
    )

    if (gpuMesh.maxBonesPerVertex > 0) {
      attribs :+= Attrib(4, DataFmt.UI8, Semantic.BoneIndex)
    }

    if (gpuMesh.maxBonesPerVertex > 1) {
      attribs :+= Attrib(4, DataFmt.UN8, Semantic.BoneWeight)
    }

    val spec = VertexSpec(attribs)
    val uvRange = gpuMesh.uvMax - gpuMesh.uvMin
    val vertexBuffer = MemoryUtil.memAlloc(mesh.vertices.length * spec.sizeInBytes)
    val indexBuffer = MemoryUtil.memAlloc(mesh.indices.length * 2)

    gpuMesh.vertexData = vertexBuffer
    gpuMesh.indexData = indexBuffer
    gpuMesh.vertexSpec = spec

    val ts = new Array[Byte](4)
    for (vert <- mesh.vertices) {
      vertexBuffer.putFloat(vert.position.x.toFloat)
      vertexBuffer.putFloat(vert.position.y.toFloat)
      vertexBuffer.putFloat(vert.position.z.toFloat)

      val relUv = (vert.uv - gpuMesh.uvMin) /@ uvRange
      val uvX = clamp((relUv.x * 0xFFFF.toDouble).toInt, 0, 0xFFFF).toShort
      val uvY = clamp((relUv.y * 0xFFFF.toDouble).toInt, 0, 0xFFFF).toShort
      vertexBuffer.putShort(uvX)
      vertexBuffer.putShort(uvY)

      ts(0) = clamp((vert.normal.x * 127.0).toInt, -127, 127).toByte
      ts(1) = clamp((vert.normal.y * 127.0).toInt, -127, 127).toByte
      ts(2) = clamp((vert.normal.z * 127.0).toInt, -127, 127).toByte
      vertexBuffer.put(ts)

      ts(0) = clamp((vert.tangent.x * 127.0).toInt, -127, 127).toByte
      ts(1) = clamp((vert.tangent.y * 127.0).toInt, -127, 127).toByte
      ts(2) = clamp((vert.tangent.z * 127.0).toInt, -127, 127).toByte
      vertexBuffer.put(ts)

      ts(0) = clamp((vert.bitangent.x * 127.0).toInt, -127, 127).toByte
      ts(1) = clamp((vert.bitangent.y * 127.0).toInt, -127, 127).toByte
      ts(2) = clamp((vert.bitangent.z * 127.0).toInt, -127, 127).toByte
      vertexBuffer.put(ts)

      if (gpuMesh.maxBonesPerVertex > 0) {
        for (boneI <- 0 until 4) {
          val bone = vert.bones.lift(boneI).getOrElse(BoneWeight(0, 0))
          vertexBuffer.put(bone.index.toByte)
        }
      }

      if (gpuMesh.maxBonesPerVertex > 1) {
        for (boneI <- 0 until 4) {
          val bone = vert.bones.lift(boneI).getOrElse(BoneWeight(0, 0))
          vertexBuffer.put(clamp((bone.weight * 255.0).toInt, 0, 255).toByte)
        }
      }
    }

    vertexBuffer.finish()
    indexBuffer.asShortBuffer.put(mesh.indices.map(_.toShort))

    gpuMesh
  }

}
