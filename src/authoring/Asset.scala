package tower.authoring

object Asset {

  def deferLoad(filename: String): Option[() => Asset] = {
    val extBegin = filename.lastIndexOf('.')
    if (extBegin == -1) return None
    val ext = filename.substring(extBegin + 1).toLowerCase

    ext match {
      case "fbx" => Some(() => new asset.AssimpAsset(filename))
      case _ => None
    }
  }

  def load(filename: String): Option[Asset] = deferLoad(filename).map(load => load())

}

abstract class Asset(val filename: String) {

  def resources: Seq[Resource]

}
