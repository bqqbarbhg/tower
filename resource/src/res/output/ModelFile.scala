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

    val Version = 1
    buffer.putMagic("s2md")
    buffer.putVersion(Version)

    buffer.putInt(model.nodes.length)
    buffer.putInt(model.meshes.length)
    buffer.putInt(model.animations.length)

    for (node <- model.nodes) {
      buffer.putIdentifier(node.node.name)
      buffer.putMatrix43(node.node.transform)
      buffer.putInt(node.parent)
    }

    for (mesh <- model.meshes) {
      buffer.putInt(mesh.parent)
      buffer.putIdentifier(mesh.mesh.name)
      buffer.putIdentifier(mesh.resource)
    }

    for (anim <- model.animations) {
      buffer.putIdentifier(anim.name)
      buffer.putIdentifier(anim.resource)
    }

    buffer.putMagic("E.md")
    buffer.finish()

    writer.writeFile(file, buffer)

    MemoryUtil.memFree(buffer)
  }
}

