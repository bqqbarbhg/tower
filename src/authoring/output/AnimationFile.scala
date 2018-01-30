package tower.authoring.output

import java.io.FileOutputStream
import java.nio.ByteBuffer

import scala.collection.mutable.ArrayBuffer
import tower.authoring.resource._
import tower.util.Serialization.ByteBufferExtension
import tower.util.SharedByteBuffer

object AnimationFile {

  def save(filename: String, animation: AnimationResource): Unit = {

    // @Serialize(s2an)

    val buffer = SharedByteBuffer.acquire()

    val Version = 1

    val data = new ArrayBuffer[Float]()

    buffer.putMagic("s2an")
    buffer.putVersion(Version)

    buffer.putIdentifier(animation.name)
    buffer.putDouble(animation.duration)

    buffer.putInt(animation.timelines.length)
    for (tl <- animation.timelines) {
      val base = data.length

      buffer.putIdentifier(tl.boneName)
      buffer.putInt(base)
      buffer.putInt(tl.rot.length)
      buffer.putInt(tl.pos.length)
      buffer.putInt(tl.size.length)

      for (rot <- tl.rot) data += rot.time.toFloat
      for (rot <- tl.rot) {
        data += rot.value.x.toFloat
        data += rot.value.y.toFloat
        data += rot.value.z.toFloat
        data += rot.value.w.toFloat
      }

      for (pos <- tl.pos) data += pos.time.toFloat
      for (pos <- tl.pos) {
        data += pos.value.x.toFloat
        data += pos.value.y.toFloat
        data += pos.value.z.toFloat
      }

      for (size <- tl.size) data += size.time.toFloat
      for (size <- tl.size) {
        data += size.value.x.toFloat
        data += size.value.y.toFloat
        data += size.value.z.toFloat
      }
    }

    buffer.putInt(data.length)
    buffer.asFloatBuffer.put(data.toArray)
    buffer.position(buffer.position + data.length * 4)

    val output = new FileOutputStream(filename)
    buffer.writeTo(output)
    output.close()

    SharedByteBuffer.release()
  }

}
