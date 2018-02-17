package res.intermediate

import java.io.File

import res.importer.Importer

/**
  * Represents an asset source file that has its configuration resolved.
  *
  * @param file Filename of the content file
  * @param config Resolved configuration
  * @param configSources List of which config files were taken into account for this asset
  */
class AssetFile(val file: File, val config: Config, val configSources: Vector[ConfigFile]) {

  /** Has the asset changed since the last processing time? */
  var hasChanged: Boolean = false

  /** Import this asset */
  def importAsset(): Iterable[Resource] = {
    val importer = Importer.get(config.importer.name)
    importer.toIterable.flatMap(_.importAsset(this))
  }
}

