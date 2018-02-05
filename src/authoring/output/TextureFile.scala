package tower.authoring.output

import java.io.FileOutputStream
import java.nio.ByteBuffer

import tower.authoring.resource.TextureResource
import tower.util.Serialization.ByteBufferExtension
import tower.util.SharedByteBuffer

object TextureFile {

  def save(filename: String, textureLevels: Seq[TextureResource]): Unit = {

    // @Serialize(s2tx)

    val buffer = SharedByteBuffer.acquire()

    val Version = 1
    buffer.putMagic("s2tx")
    buffer.putVersion(Version)

    val base = textureLevels.head

    buffer.putIdentifier(base.name)
    buffer.putInt(base.width)
    buffer.putInt(base.height)
    buffer.putInt(textureLevels.length)
    buffer.putMagic(base.format)

    for (level <- textureLevels) {
      assert(level.format == base.format)

      val dataToCopy = level.data.duplicateEx
      buffer.putInt(level.data.remaining)
      buffer.put(dataToCopy)
    }

    buffer.putMagic("E.tx")

    val output = new FileOutputStream(filename)
    buffer.writeTo(output)
    output.close()

    SharedByteBuffer.release(buffer)
  }
}
