package res.output

import java.io.File
import java.nio.ByteBuffer

import core._
import org.lwjgl.system.MemoryUtil
import res.intermediate._
import util.BufferUtils._
import res.runner.OutputFileWriter

object MeshFile {

  private def saveMesh(buffer: ByteBuffer, mesh: GpuMesh): Unit = {
    // @Serialize(s2mp)

    val Version = 1
    buffer.putMagic("s2mp")
    buffer.putVersion(Version)

    buffer.putInt(mesh.numVertices)
    buffer.putInt(mesh.numIndices)
    buffer.putInt(mesh.bones.length)

    for (bone <- mesh.bones) {
      buffer.putIdentifier(bone.name)
      buffer.putMatrix43(bone.meshToBone)
    }

    buffer.putInt(mesh.vertexSpec.attribs.length)
    for (attrib <- mesh.vertexSpec.attribs) {
      buffer.putMagic(attrib.fmt.magic)
      buffer.putMagic(attrib.semantic.magic)
      buffer.putShort(attrib.num.toShort)
      buffer.putShort(attrib.index.toShort)
    }

    buffer.putInt(mesh.vertexData.remaining)
    buffer.putInt(mesh.indexData.remaining)

    buffer.put(mesh.vertexData.duplicateEx)
    buffer.put(mesh.indexData.duplicateEx)

    buffer.putMagic("E.mp")
  }

  def save(writer: OutputFileWriter, file: File, parts: Seq[GpuMesh]): Unit = {
    // @Serialize(s2ms)

    val buffer = MemoryUtil.memAlloc(64 * 1024 * 1024)

    val Version = 1
    buffer.putMagic("s2ms")
    buffer.putVersion(1)

    buffer.putInt(parts.length)

    for (part <- parts) {
      saveMesh(buffer, part)
    }

    buffer.putMagic("E.ms")
    buffer.finish()

    writer.writeFile(file, buffer)

    MemoryUtil.memFree(buffer)
  }
}
