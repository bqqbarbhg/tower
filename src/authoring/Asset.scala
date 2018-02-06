package tower.authoring

object Asset {

  def deferLoad(filename: String, baseName: String): Option[() => Asset] = {
    val extBegin = filename.lastIndexOf('.')
    if (extBegin == -1) return None
    val ext = filename.substring(extBegin + 1).toLowerCase

    ext match {
      case "fbx" => Some(() => new asset.AssimpAsset(filename, baseName))
      case "png" => Some(() => new asset.StbiAsset(filename, baseName))
      case "jpg" => Some(() => new asset.StbiAsset(filename, baseName))
      case "ogg" => Some(() => new asset.VorbisAsset(filename, baseName))
      case _ => None
    }
  }

  def load(filename: String, baseName: String): Option[Asset] = deferLoad(filename, baseName).map(load => load())

}

abstract class Asset(val filename: String, val baseName: String) {

  def resources: Seq[Resource]

}
