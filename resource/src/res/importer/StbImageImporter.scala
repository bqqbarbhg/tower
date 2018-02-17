package res.importer

import org.lwjgl.stb.STBImage
import res.intermediate._

object StbImageImporter extends Importer {
  override def importType: ImportFileType = ImportFileImage

  def importAsset(asset: AssetFile): Iterable[Resource] = {
    val wa = Array(0)
    val ha = Array(0)
    val ca = Array(0)
    val filename = asset.file.getAbsolutePath
    val data = STBImage.stbi_load(filename, wa, ha, ca, 4)
    val width = wa(0)
    val height = ha(0)

    val srgb = asset.config.res.image.srgb
    val image = Image.create32(width, height, srgb, data)

    STBImage.stbi_image_free(data)

    Some(image)
  }

}
