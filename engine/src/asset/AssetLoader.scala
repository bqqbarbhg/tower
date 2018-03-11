package asset

import core._

import collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.reflect.ClassTag

object AssetLoader {
  private val assets = new mutable.HashSet[LoadableAsset]()
  private val namedAssets = new mutable.HashMap[Identifier, LoadableAsset]()

  def add(asset: LoadableAsset): Unit = {
    assets += asset
  }

  def getOrAdd[T <: LoadableAsset : ClassTag](name: Identifier, ctor: => T): T = {
    namedAssets.getOrElseUpdate(name, {
      val asset = ctor
      namedAssets(name) = asset
      asset
    }).asInstanceOf[T]
  }

  def reloadEverything(): Unit = {
    val preloaded = assets.toSeq.filter(_.isPreloaded)
    val loaded = assets.toSeq.filter(_.isLoaded)

    // Unload everything
    for (asset <- (preloaded ++ loaded))
      asset.unload()

    // Restore assets to their original state
    for (asset <- preloaded) asset.preload()
    for (asset <- loaded) asset.load()
  }

}

