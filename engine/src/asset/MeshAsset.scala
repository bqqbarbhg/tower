package asset

import gfx._
import core._

class MeshAsset(val name: Identifier) extends LoadableAsset {

  private var meshImpl: Mesh = null

  def get: Mesh = {
    load()
    meshImpl
  }

  override def loadAsset(): Unit = {
    // @Todo: What to do about failed loads?
    meshImpl = Mesh.load(name).get
  }

  override def unloadAsset(): Unit = {
    meshImpl.unload()
    meshImpl = null
  }

}


