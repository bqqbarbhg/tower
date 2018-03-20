package asset

import gfx._
import core._

object AnimationAsset {
  def apply(name: String): AnimationAsset = apply(Identifier(name))
  def apply(name: Identifier): AnimationAsset = AssetLoader.getOrAdd(name, new AnimationAsset(name))
}

class AnimationAsset(val name: Identifier) extends LoadableAsset {
  def debugName: String = s"Animation: $name"

  private var animImpl: Animation = null

  def get: Animation = {
    load()
    animImpl
  }

  override def loadAsset(): Unit = {
    // @Todo: What to do about failed loads?
    animImpl = Animation.load(name).get
  }

  override def unloadAsset(): Unit = {
    animImpl.unload()
    animImpl = null
  }

}

