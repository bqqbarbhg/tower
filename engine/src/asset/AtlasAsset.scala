package asset

import ui._
import core._
import task.Task
import ui.Sprite.SpriteMap

object AtlasAsset {
  def apply(name: String): AtlasAsset = apply(Identifier(name))
  def apply(name: Identifier): AtlasAsset = AssetLoader.getOrAdd(name, new AtlasAsset(name))
}

class AtlasAsset(val name: Identifier) extends LoadableAsset {
  def debugName: String = s"Atlas: $name"

  private var loadTask: Task[Unit] = null
  private var atlasImpl: Atlas = null

  def get: Atlas = {
    load()
    atlasImpl
  }

  override def preloadAsset(): Iterable[LoadableAsset] = {
    // @Todo: What to do about failed loads?
    atlasImpl = Atlas.load(name, SpriteMap.atlasAssets.length).get
    SpriteMap.atlasAssets += this
    None
  }

  override def isAssetLoaded() = loadTask.isCompleted

  override def startLoadingAsset(): Unit = {
    loadTask = atlasImpl.loadTextures()
  }

  override def loadAsset(): Unit = {
    loadTask.get
  }

  override def unloadAsset(): Unit = {
    atlasImpl.unload()
    atlasImpl = null
  }

}


