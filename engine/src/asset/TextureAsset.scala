package asset

import gfx._
import core._

object TextureAsset {
  def apply(name: String): TextureAsset = apply(Identifier(name))
  def apply(name: Identifier): TextureAsset = AssetLoader.getOrAdd(name, new TextureAsset(name))
}

class TextureAsset(val name: Identifier) extends LoadableAsset {

  private var texImpl: Texture = null

  def get: Texture = {
    load()
    texImpl
  }

  override def loadAsset(): Unit = {
    // @Todo: What to do about failed loads?
    texImpl = Texture.load(name).get
  }

  override def unloadAsset(): Unit = {
    texImpl.unload()
    texImpl = null
  }

}


