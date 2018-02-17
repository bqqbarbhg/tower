package res.intermediate

import java.nio.ByteBuffer

import org.lwjgl.system.MemoryUtil

object Texture {

  object Format {
    val Rgba = "RGBA"
    val Dxt1 = "DXT1"
    val Dxt5 = "DXT5"
  }

}

class Texture(val width: Int, val height: Int, val format: String) extends Resource {

  /** Data of the mip-map levels, allocated using `MemoryUtil` */
  var levelData: Array[ByteBuffer] = Array[ByteBuffer]()

  override def unload(): Unit = {
    levelData.foreach(MemoryUtil.memFree)
    levelData = Array[ByteBuffer]()
  }

}
