package tower.authoring.resource

import java.nio.ByteBuffer

object TextureResource {

  object Format {
    val Rgba = "RGBA"
    val Dxt1 = "DXT1"
    val Dxt5 = "DXT5"
  }

}

class TextureResource(name: String) extends tower.authoring.Resource(name) {

  var format: String = ""
  var width: Int = 0
  var height: Int = 0
  var data: ByteBuffer = null

}
