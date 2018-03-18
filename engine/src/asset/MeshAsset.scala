package asset

import gfx._
import core._
import task.Task

class MeshAsset(val name: Identifier) extends LoadableAsset {

  private var loadTask: Task[Mesh] = null
  private var meshImpl: Mesh = null

  def get: Mesh = {
    load()
    meshImpl
  }

  override def isAssetLoaded() = loadTask.isCompleted

  override def startLoadingAsset(): Unit = {
    loadTask = Mesh.deferredLoad(name)
  }

  override def loadAsset(): Unit = {
    // @Todo: What to do about failed loads?
    meshImpl = loadTask.get
  }

  override def unloadAsset(): Unit = {
    meshImpl.unload()
    meshImpl = null
  }

}


