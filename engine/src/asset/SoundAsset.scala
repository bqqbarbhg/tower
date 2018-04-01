package asset

import audio._
import core._
import task.Task

object SoundAsset {
  def apply(name: String): SoundAsset = apply(Identifier(name))
  def apply(name: Identifier): SoundAsset = AssetLoader.getOrAdd(name, new SoundAsset(name))
}

class SoundAsset(val name: Identifier) extends LoadableAsset {
  def debugName: String = s"Sound: $name"

  private var loadTask: Task[Sound] = null
  private var soundImpl: Sound = null

  def get: Sound = {
    load()
    soundImpl
  }

  override def isAssetLoaded() = loadTask.isCompleted

  override def startLoadingAsset(): Unit = {
    loadTask = Sound.deferredLoad(name)
  }

  override def loadAsset(): Unit = {
    // @Todo: What to do about failed loads?
    soundImpl = loadTask.get
  }

  override def unloadAsset(): Unit = {
    soundImpl.unload()
    soundImpl = null
  }

}


