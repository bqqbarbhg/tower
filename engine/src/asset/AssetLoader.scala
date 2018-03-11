package asset

import core._
import ui.Sprite.SpriteMap

import collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.reflect.ClassTag
import io.content.Package

object AssetLoader {
  private val assets = new mutable.HashSet[LoadableAsset]()
  private val namedAssets = new mutable.HashMap[Identifier, LoadableAsset]()
  private val atlases = new ArrayBuffer[AtlasAsset]()

  def add(asset: LoadableAsset): Unit = {
    assets += asset

    asset match {
      case atlas: AtlasAsset => atlases += atlas
      case _ =>
    }
  }

  def getOrAdd[T <: LoadableAsset : ClassTag](name: Identifier, ctor: => T): T = {
    namedAssets.getOrElseUpdate(name, {
      val asset = ctor
      namedAssets(name) = asset
      asset
    }).asInstanceOf[T]
  }

  def preloadAtlases(): Unit = {
    val atlasFiles = Package.get.list("atlas").filter(_.name.endsWith(".s2at"))
    for (atlasFile <- atlasFiles) {
      AtlasAsset(atlasFile.name)
    }

    SpriteMap.clear()
    for (atlas <- atlases) {
      atlas.preload()
    }
  }

  def reloadEverything(): Unit = {
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
  }

}

