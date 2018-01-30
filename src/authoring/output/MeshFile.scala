package tower.authoring.output

import java.io.FileOutputStream
import java.nio.ByteBuffer

import tower.authoring.resource._
import tower.util.Serialization.ByteBufferExtension
import tower.math._
import tower.util.SharedByteBuffer

object MeshFile {

  def save(filename: String, mesh: MeshResource): Unit = {

    // @Serialize(s2ms)

    val buffer = SharedByteBuffer.acquire()

    val Version = 1
    buffer.putMagic("s2ms")
    buffer.putVersion(Version)

    buffer.putIdentifier(mesh.name)
    buffer.putInt(mesh.vertices.length)
    buffer.putInt(mesh.indices.length)

    val quat = new Array[Byte](4)

    for (vert <- mesh.vertices) {
      buffer.putFloat(vert.position.x.toFloat)
      buffer.putFloat(vert.position.y.toFloat)
      buffer.putFloat(vert.position.z.toFloat)
      buffer.putFloat(vert.uv.x.toFloat)
      buffer.putFloat(vert.uv.y.toFloat)

      quat(0) = clamp((vert.tangentSpace.x * 128.0).toInt, -128, 127).toByte
      quat(1) = clamp((vert.tangentSpace.y * 128.0).toInt, -128, 127).toByte
      quat(2) = clamp((vert.tangentSpace.z * 128.0).toInt, -128, 127).toByte
      quat(3) = clamp((vert.tangentSpace.w * 128.0).toInt, -128, 127).toByte
      buffer.put(quat)

      for (boneI <- 0 until 4) {
        val bone = vert.bones.lift(boneI).getOrElse(BoneWeight(0, 0))
        buffer.put(bone.index.toByte)
        buffer.put(clamp((bone.weight * 255.0).toInt, 0, 255).toByte)
      }
    }

    for (index <- mesh.indices) {
      buffer.putShort(index.toShort)
    }

    val output = new FileOutputStream(filename)
    buffer.writeTo(output)
    output.close()

    SharedByteBuffer.release()
  }

}

