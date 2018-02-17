package res.importer

import res.intermediate._

object Importer {

  def get(name: String): Option[Importer] = {
    name match {
      case "stb_image" => Some(StbImageImporter)
      case _ => None
    }
  }

}

trait Importer {

  /** Type of resource this importer loads */
  def importType: ImportFileType

  /** Import the resources out of the asset */
  def importAsset(asset: AssetFile): Iterable[Resource]

}