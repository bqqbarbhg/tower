package tower.authoring.output

import java.io.FileOutputStream
import java.nio.ByteBuffer

import tower.authoring.resource.TextureResource
import tower.util.Serialization.ByteBufferExtension
import tower.util.SharedByteBuffer

object TextureFile {

  def save(filename: String, texture: TextureResource): Unit = {

    // @Serialize(s2tx)

    val buffer = SharedByteBuffer.acquire()

    val Version = 1
    buffer.putMagic("s2tx")
    buffer.putVersion(Version)

    buffer.putIdentifier(texture.name)
    buffer.putInt(texture.width)
    buffer.putInt(texture.height)
    buffer.putInt(texture.data.remaining)
    buffer.putMagic(texture.format)

    val dataToCopy = texture.data.duplicateEx
    buffer.put(dataToCopy)

    buffer.putMagic("E.tx")

    val output = new FileOutputStream(filename)
    buffer.writeTo(output)
    output.close()

    SharedByteBuffer.release(buffer)
  }
}
