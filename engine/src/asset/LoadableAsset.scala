package asset

import core._
import LoadableAsset._

object LoadableAsset {
  val StateUndefined = 0
  val StateUnloaded = 1
  val StatePreloaded = 2
  val StateLoadQueued = 3
  val StateLoading = 4
  val StateLoaded = 5
}

/**
  * Represents an abstract loadable asset. The load may be deferred so that it
  * happens (much) later than the creation of the asset object itself.
  */
abstract class LoadableAsset {
  AssetLoader.add(this)

  private var state = StateUnloaded
  private var dependencies: Array[LoadableAsset] = Array[LoadableAsset]()
  private var refcountState = 0

  protected def preloadAsset(): Iterable[LoadableAsset] = None
  protected def startLoadingAsset(): Unit = { }
  protected def isAssetLoaded(): Boolean = true
  protected def loadAsset(): Unit
  protected def unloadAsset(): Unit

  protected final def isAssetAndDependenciesLoaded(): Boolean = isAssetLoaded() && dependencies.forall(_.isLoaded)

  final def isUnloaded: Boolean = state == StateUnloaded
  final def isPreloaded: Boolean = state == StatePreloaded
  final def isLoadQueued: Boolean = state == StateLoadQueued
  final def isLoading: Boolean = state == StateLoading
  final def isLoaded: Boolean = state == StateLoaded

  final def isReferenced: Boolean = refcountState > 0
  final def acquire(): Unit = { refcountState += 1 }
  final def release(): Unit = { refcountState -= 1 }

  /**
    * Load the header of the object and collect dependencies.
    */
  final def preload(): Unit = {
    if (state == StateUnloaded) {
      dependencies = preloadAsset().toArray
      state = StatePreloaded
    }
  }

  /**
    * Queue load on this file an it's dependencies.
    */
  final def queueLoad(): Unit = {
    preload()
    if (state == StatePreloaded) {
      for (dep <- dependencies) {
        dep.acquire()
        dep.queueLoad()
      }
      state = StateLoadQueued
    }
  }

  /**
    * Start loading the asset in the background.
    */
  final def startLoading(): Unit = {
    queueLoad()
    if (state == StateLoadQueued) {
      startLoadingAsset()
      state = StateLoading
    }
  }

  /**
    * Try finishing the load if possible.
    */
  final def tryFinishLoading(): Boolean = {
    if (isAssetAndDependenciesLoaded()) {
      load()
      true
    } else {
      false
    }
  }

  /**
    * Load the actual content of the asset.
    */
  final def load(): Unit = {
    startLoading()
    if (state == StateLoading) {
      for (dep <- dependencies) {
        dep.load()
      }
      loadAsset()
      state = StateLoaded
    }
  }

  /**
    * Release the content used by the asset.
    */
  final def unload(): Unit = {
    if (state == StateLoading) {
      load()
      state = StateLoaded
    }
    if (state == StateLoaded) {
      unloadAsset()
      state = StateLoadQueued
    }
    if (state == StateLoadQueued) {
      for (dep <- dependencies)
        dep.release()
      state = StatePreloaded
    }
    if (state == StatePreloaded) {
      state = StateUnloaded
      dependencies = Array[LoadableAsset]()
    }
  }

}

