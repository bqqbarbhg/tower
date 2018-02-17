package res.runner

import java.io.File

/**
  * Represents an asset source file that has its configuration resolved.
  *
  * @param file Filename of the content file
  * @param config Resolved configuration
  * @param configSources List of which config files were taken into account for this asset
  */
class AssetFile(val file: File, val config: Config, val configSources: Vector[ConfigFile]) {

  /** Does this asset need to be processed? */
  var needsProcessing: Boolean = false

}

