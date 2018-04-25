package asset

import core._
import ui.Sprite.SpriteMap

import collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.collection.immutable
import scala.reflect.ClassTag
import io.content.Package
import locale.Locale

object AssetLoader {
  private val assets = new mutable.HashSet[LoadableAsset]()
  private val namedAssets = new mutable.HashMap[Identifier, LoadableAsset]()
  private val atlases = new ArrayBuffer[AtlasAsset]()

  def add(asset: LoadableAsset): Unit = this.synchronized {
    assets += asset

    asset match {
      case atlas: AtlasAsset => atlases += atlas
      case _ =>
    }
  }

  def getOrAdd[T <: LoadableAsset : ClassTag](name: Identifier, ctor: => T): T = this.synchronized {
    namedAssets.getOrElseUpdate(name, {
      val asset = ctor
      namedAssets(name) = asset
      asset
    }).asInstanceOf[T]
  }

  def preloadAtlases(): Unit = this.synchronized {
    val atlasFiles = Package.get.list("atlas").filter(_.name.endsWith(".s2at"))
    for (atlasFile <- atlasFiles) {
      AtlasAsset(atlasFile.name)
    }

    SpriteMap.clear()
    for (atlas <- atlases) {
      atlas.preload()
    }
  }

  def reloadEverything(): Unit = this.synchronized {
    val preloaded = assets.toSeq.filter(_.isPreloaded)
    val loaded = assets.toSeq.filter(_.isLoaded)

    // Unload everything
    for (asset <- (preloaded ++ loaded))
      asset.unload()

    // Preload atlases to map sprites
    preloadAtlases()

    // Restore assets to their original state
    for (asset <- preloaded) asset.preload()
    for (asset <- loaded) asset.load()

    // Reload locales
    if (Locale.instance != null) {
      Locale.load(Locale.instance.filename)
    }
  }

  def unloadEverything(): Unit = this.synchronized {
    for (asset <- assets) {
      asset.unload()
    }
  }

  def startLoading(): ArrayBuffer[LoadableAsset] = this.synchronized {

    // Queue load on all referenced assets. Since the preload() phase may
    // generate new assets we may need to go over all the assets multiple
    // times.
    var immutableAssets: immutable.Set[LoadableAsset] = null
    do {
      immutableAssets = assets.toSet
      for (asset <- assets) {
        if (asset.isReferenced) {
          asset.queueLoad()
        }
      }
    } while (immutableAssets.size != assets.size)

    val queuedLoads = new ArrayBuffer[LoadableAsset]()
    val currentlyLoading = assets.filter(_.isLoading)

    // Start loading queued assets
    for (asset <- assets) {
      if (asset.isLoadQueued) {
        queuedLoads += asset
        asset.startLoading()
      }
    }

    queuedLoads ++= currentlyLoading

    queuedLoads
  }

}

