package asset

import audio._
import core._

object SoundAsset {
  def apply(name: String): SoundAsset = apply(Identifier(name))
  def apply(name: Identifier): SoundAsset = AssetLoader.getOrAdd(name, new SoundAsset(name))
}

class SoundAsset(val name: Identifier) extends LoadableAsset {
  def debugName: String = s"Sound: $name"

  private var soundImpl: Sound = null

  def get: Sound = {
    load()
    soundImpl
  }

  override def loadAsset(): Unit = {
    // @Todo: What to do about failed loads?
    soundImpl = Sound.load(name).get
  }

  override def unloadAsset(): Unit = {
    soundImpl.unload()
    soundImpl = null
  }

}


