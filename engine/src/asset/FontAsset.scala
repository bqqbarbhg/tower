package asset

import ui._
import core._

object FontAsset {
  def apply(name: String): FontAsset = apply(Identifier(name))
  def apply(name: Identifier): FontAsset = AssetLoader.getOrAdd(name, new FontAsset(name))
}

class FontAsset(val name: Identifier) extends LoadableAsset {
  def debugName: String = s"Font: $name"

  private var fontImpl: Font = null

  def get: Font = {
    load()
    fontImpl
  }

  override def loadAsset(): Unit = {
    // @Todo: What to do about failed loads?
    fontImpl = Font.load(name).get
  }

  override def unloadAsset(): Unit = {
    fontImpl.unload()
    fontImpl = null
  }

}


