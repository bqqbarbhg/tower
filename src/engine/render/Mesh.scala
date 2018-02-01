package tower.engine.render

import java.io.InputStream
import java.nio.ByteBuffer

import org.lwjgl.opengl.GL11._
import org.lwjgl.opengl.GL15._
import org.lwjgl.opengl.GL20._
import org.lwjgl.opengl.GL30._

import tower.util.Serialization.ByteBufferExtension
import tower.Identifier

class Mesh {

  var name: Identifier = Identifier.Empty
  var parts: Array[MeshPart] = Array[MeshPart]()

  def load(buffer: ByteBuffer): Unit = {

    // @Serialize(s2ms)

    // File header
    buffer.verifyMagic("s2ms")
    val MaxVersion = 1
    val version = buffer.getVersion(MaxVersion)

    // Mesh header
    this.name = buffer.getIdentifier()
    val numParts = buffer.getInt()

    // Mesh parts
    this.parts = new Array[MeshPart](numParts)
    for (i <- 0 until parts.length) {
      val part = new MeshPart()
      part.load(buffer)
      parts(i) = part
    }

    buffer.verifyMagic("E.ms")
  }


}
