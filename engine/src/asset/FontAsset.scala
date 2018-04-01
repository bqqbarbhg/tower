package asset

import ui._
import core._
import task.Task

object FontAsset {
  def apply(name: String): FontAsset = apply(Identifier(name))
  def apply(name: Identifier): FontAsset = AssetLoader.getOrAdd(name, new FontAsset(name))
}

class FontAsset(val name: Identifier) extends LoadableAsset {
  def debugName: String = s"Font: $name"

  private var loadTask: Task[Font] = null
  private var fontImpl: Font = null

  def get: Font = {
    load()
    fontImpl
  }

  override def isAssetLoaded() = loadTask.isCompleted

  override def startLoadingAsset(): Unit = {
    loadTask = Font.deferredLoad(name)
  }

  override def loadAsset(): Unit = {
    // @Todo: What to do about failed loads?
    fontImpl = loadTask.get
  }

  override def unloadAsset(): Unit = {
    fontImpl.unload()
    fontImpl = null
  }

}


