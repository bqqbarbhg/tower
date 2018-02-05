package tower.authoring.asset

import org.lwjgl.stb.STBImage

import tower.authoring.Asset
import tower.authoring.Resource
import tower.authoring.resource._

class StbiAsset(filename: String, baseName: String) extends Asset(filename, baseName) {

  private var image = new ImageResource(s"$baseName.s2tx")

  {
    val wa = Array(0)
    val ha = Array(0)
    val ca = Array(0)
    val data = STBImage.stbi_load(filename, wa, ha, ca, 4)
    val width = wa(0)
    val height = ha(0)

    image.width = width
    image.height = height
    image.data = new Array[Int](data.remaining)

    val numBytes = width * height
    for (i <- 0 until numBytes) {
      val r = data.get(i * 4 + 0)
      val g = data.get(i * 4 + 1)
      val b = data.get(i * 4 + 2)
      val a = data.get(i * 4 + 3)
      image.data(i) = Pixel(r, g, b, a).bits
    }

    STBImage.stbi_image_free(data)
  }

  def resources: Seq[Resource] = Array(image)
}
