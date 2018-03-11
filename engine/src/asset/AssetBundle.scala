package asset

class AssetBundle(assets: Vector[LoadableAsset]) extends LoadableAsset {
  def this(assets: LoadableAsset*) = this(assets.toVector)

  override def preloadAsset(): Iterable[LoadableAsset] = assets
  override def loadAsset(): Unit = { }
  override def unloadAsset(): Unit = { }
}
