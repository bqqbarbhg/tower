package asset

class AssetBundle(name: String, assets: Vector[LoadableAsset]) extends LoadableAsset {
  def debugName: String = s"Bundle: $name"

  def this(name: String, assets: LoadableAsset*) = this(name, assets.toVector)

  override def preloadAsset(): Iterable[LoadableAsset] = assets
  override def loadAsset(): Unit = { }
  override def unloadAsset(): Unit = { }
}
