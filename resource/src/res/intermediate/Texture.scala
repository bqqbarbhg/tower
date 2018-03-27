package res.intermediate

import java.nio.ByteBuffer

import org.lwjgl.system.MemoryUtil

object Texture {

  object Format {
    val Rgba = "RGBA"
    val Rg = "RG.."
    val Dxt1 = "DXT1"
    val Dxt5 = "DXT5"
    val Bc4 = "BC4."
    val Bc5 = "BC5."
    val Rgba16 = "Ri16"
    val Rgb16 = "Rb16"
  }

}

class Texture(val width: Int, val height: Int, val format: String, val readAsLinear: Option[Boolean] = None) extends Resource {

  /** Data of the mip-map levels, allocated using `MemoryUtil` */
  var levelData: Array[ByteBuffer] = Array[ByteBuffer]()

  override def unload(): Unit = {
    levelData.foreach(MemoryUtil.memFree)
    levelData = Array[ByteBuffer]()
  }

}
