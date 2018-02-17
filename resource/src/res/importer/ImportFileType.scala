package res.importer

import res.intermediate._

/** What kind of resources are found in this file type? */
sealed abstract class ImportFileType {

  /**
    * Clear out the parts of the config that are not relevant for this file type.
    * Allows for more sloppy handling of configration, for example setting all the
    * assets under a directory to have `image.type = "sprite"` would cause the
    * non-image assets to be treated as sprites as well otherwise.
    * This also improves caching efficency as unrelated config changes don't
    * cause assets to be re-processed.
    */
  def maskConfig(config: Config): Unit

}

object ImportFileImage extends ImportFileType {
  def maskConfig(config: Config): Unit = {
    config.res.animation = new Config.Res.Animation()
  }
}

object ImportFileModel extends ImportFileType {
  def maskConfig(config: Config): Unit = {
    config.res.image = new Config.Res.Image()
    config.res.texture = new Config.Res.Texture()
    config.res.sprite = new Config.Res.Sprite()
    config.res.atlas = new Config.Res.Atlas()
  }
}
