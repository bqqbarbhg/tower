package res.output

import java.io.File
import java.nio.ByteBuffer

import core._
import org.lwjgl.system.MemoryUtil
import res.intermediate._
import util.BufferUtils._
import res.runner.OutputFileWriter

object ModelFile {

  def save(writer: OutputFileWriter, file: File, model: FlatModel): Unit = {
    // @Serialize(s2mp)

    val buffer = MemoryUtil.memAlloc(64 * 1024 * 1024)

    var numNonAuxilary = model.nodes.indexWhere(_.auxilary)
    if (numNonAuxilary < 0) numNonAuxilary = model.nodes.length

    val Version = 1
    buffer.putMagic("s2md")
    buffer.putVersion(Version)

    buffer.putInt(model.nodes.length)
    buffer.putInt(model.meshes.length)
    buffer.putInt(model.animations.length)
    buffer.putInt(model.materials.length)
    buffer.putInt(numNonAuxilary)

    for (node <- model.nodes) {
      buffer.putIdentifier(node.node.name)
      buffer.putInt(node.parent)

      var flags = 0x00
      if (node.auxilary) flags |= 0x01
      buffer.putInt(flags)

      val affine = node.node.transform.toAffine
      buffer.putAffine(affine)
    }

    for (mesh <- model.meshes) {
      buffer.putInt(mesh.parent)
      buffer.putIdentifier(mesh.mesh.name)
      buffer.putIdentifier(mesh.resource)
      buffer.putInt(mesh.materialIndex)
    }

    for (anim <- model.animations) {
      buffer.putIdentifier(anim.name)
      buffer.putIdentifier(anim.resource)
    }

    for (mat <- model.materials) {
      buffer.putIdentifier(mat.albedo)
      buffer.putIdentifier(mat.normal)
      buffer.putIdentifier(mat.roughness)
      buffer.putIdentifier(mat.metallic)
      buffer.putIdentifier(mat.ao)
    }

    buffer.putMagic("E.md")
    buffer.finish()

    writer.writeFile(file, buffer)

    MemoryUtil.memFree(buffer)
  }
}

