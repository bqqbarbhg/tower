package tower.authoring.output

import java.io.FileOutputStream
import java.nio.ByteBuffer

import tower.authoring.resource._
import tower.util.Serialization.ByteBufferExtension
import tower.math._
import tower.util.SharedByteBuffer

object MeshFile {

  def saveMesh(buffer: ByteBuffer, mesh: MeshResource): Unit = {
    // @Serialize(s2mp)

    val Version = 1
    buffer.putMagic("s2mp")
    buffer.putVersion(Version)

    buffer.putInt(mesh.vertices.length)
    buffer.putInt(mesh.indices.length)
    buffer.putInt(mesh.boneNames.length)

    for (name <- mesh.boneNames) {
      buffer.putIdentifier(name)
    }

    val quat = new Array[Byte](4)

    for (vert <- mesh.vertices) {
      buffer.putFloat(vert.position.x.toFloat)
      buffer.putFloat(vert.position.y.toFloat)
      buffer.putFloat(vert.position.z.toFloat)
      buffer.putFloat(vert.uv.x.toFloat)
      buffer.putFloat(vert.uv.y.toFloat)

      quat(0) = clamp((vert.tangentSpace.x * 128.0).toInt, -127, 127).toByte
      quat(1) = clamp((vert.tangentSpace.y * 128.0).toInt, -127, 127).toByte
      quat(2) = clamp((vert.tangentSpace.z * 128.0).toInt, -127, 127).toByte
      quat(3) = clamp((vert.tangentSpace.w * 128.0).toInt, -127, 127).toByte
      buffer.put(quat)

      for (boneI <- 0 until 4) {
        val bone = vert.bones.lift(boneI).getOrElse(BoneWeight(0, 0))
        buffer.put(bone.index.toByte)
      }

      for (boneI <- 0 until 4) {
        val bone = vert.bones.lift(boneI).getOrElse(BoneWeight(0, 0))
        buffer.put(clamp((bone.weight * 255.0).toInt, 0, 255).toByte)
      }
    }

    for (index <- mesh.indices) {
      buffer.putShort(index.toShort)
    }

    buffer.putMagic("E.mp")
  }

  def save(filename: String, name: String, parts: Seq[MeshResource]): Unit = {
    // @Serialize(s2ms)

    val buffer = SharedByteBuffer.acquire()

    val Version = 1
    buffer.putMagic("s2ms")
    buffer.putVersion(1)

    buffer.putIdentifier(name)
    buffer.putInt(parts.length)

    for (part <- parts) {
      saveMesh(buffer, part)
    }

    buffer.putMagic("E.ms")

    val output = new FileOutputStream(filename)
    buffer.writeTo(output)
    output.close()

    SharedByteBuffer.release(buffer)
  }

}

