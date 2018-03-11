package asset

import core._
import LoadableAsset._

object LoadableAsset {
  val StateUndefined = 0
  val StateUnloaded = 1
  val StatePreloaded = 2
  val StateLoaded = 3
}

/**
  * Represents an abstract loadable asset. The load may be deferred so that it
  * happens (much) later than the creation of the asset object itself.
  */
abstract class LoadableAsset {
  AssetLoader.add(this)

  private var state = StateUnloaded
  private var dependencies: Seq[LoadableAsset] = Vector[LoadableAsset]()
  private var refcountState = 0

  protected def preloadAsset(): Iterable[LoadableAsset] = None
  protected def loadAsset(): Unit
  protected def unloadAsset(): Unit

  final def isUnloaded: Boolean = state == StateUnloaded
  final def isPreloaded: Boolean = state == StatePreloaded
  final def isLoaded: Boolean = state == StateLoaded

  def acquire(): Unit = { refcountState += 1 }
  def release(): Unit = { refcountState -= 1 }

  /**
    * Load the header of the object and collect dependencies.
    *
    * State transitions:
    * - Unloaded -> Preloaded
    */
  final def preload(): Unit = {
    if (state == StateUnloaded) {
      dependencies = preloadAsset().toVector
      state = StatePreloaded
    }
  }

  /**
    * Load the actual content of the asset.
    *
    * State transitions:
    * - Unloaded  -> Loaded
    * - Preloaded -> Loaded
    */
  final def load(): Unit = {
    preload()
    if (state == StatePreloaded) {
      for (dep <- dependencies) {
        dep.acquire()
        dep.load()
      }
      loadAsset()
      state = StateLoaded
    }
  }

  /**
    * Release the content used by the asset.
    *
    * State transitions:
    * - Loaded    -> Unloaded
    * - Preloaded -> Unloaded
    */
  final def unload(): Unit = {
    if (state == StateLoaded) {
      unloadAsset()
      state = StatePreloaded
      for (dep <- dependencies) dep.release()
    }
    if (state == StatePreloaded) {
      state = StateUnloaded
      dependencies = Vector[LoadableAsset]()
    }
  }

}

