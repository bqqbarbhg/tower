package asset

import gfx._
import core._
import task.Task

object TextureAsset {
  def apply(name: String): TextureAsset = apply(Identifier(name))
  def apply(name: Identifier): TextureAsset = AssetLoader.getOrAdd(name, new TextureAsset(name))
}

class TextureAsset(val name: Identifier) extends LoadableAsset {
  def debugName: String = s"Texture: $name"

  private var loadTask: Task[Texture] = null
  private var texImpl: Texture = null

  def get: Texture = {
    load()
    texImpl
  }

  override def isAssetLoaded() = loadTask.isCompleted

  override def startLoadingAsset(): Unit = {
    loadTask = Texture.deferredLoad(name)
  }

  override def loadAsset(): Unit = {
    // @Todo: What to do about failed loads?
    texImpl = loadTask.get
  }

  override def unloadAsset(): Unit = {
    texImpl.unload()
    texImpl = null
  }

}


