package asset

import core._
import gfx._

object ModelAsset {
  def apply(name: String): ModelAsset = apply(Identifier(name))
  def apply(name: Identifier): ModelAsset = AssetLoader.getOrAdd(name, new ModelAsset(name))
}

class ModelAsset(val name: Identifier) extends LoadableAsset {

  private var modelImpl: Model = null
  private var meshes = Vector[MeshAsset]()

  def get: Model = {
    load()
    modelImpl
  }

  override def preloadAsset(): Iterable[LoadableAsset] = {
    // @Todo: What to do about failed loads?
    modelImpl = Model.load(name).get

    meshes = modelImpl.meshResource.map(m => new MeshAsset(new Identifier(m))).toVector

    meshes
  }

  override def loadAsset(): Unit = {
    for (i <- meshes.indices) {
      modelImpl.meshes(i) = meshes(i).get
    }
  }

  override def unloadAsset(): Unit = {
    modelImpl = null
    meshes = Vector[MeshAsset]()
  }
}
