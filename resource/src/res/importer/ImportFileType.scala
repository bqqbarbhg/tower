package res.importer

import res.intermediate._

/** What kind of resources are found in this file type? */
sealed abstract class ImportFileType {

  /** If the version is updated it forces a re-processing of assets of this type */
  def version: Int

  /**
    * Clear out the parts of the config that are not relevant for this file type.
    * Allows for more sloppy handling of configration, for example setting all the
    * assets under a directory to have `image.type = "sprite"` would cause the
    * non-image assets to be treated as sprites as well otherwise.
    * This also improves caching efficency as unrelated config changes don't
    * cause assets to be re-processed.
    */
  def maskConfig(config: Config): Unit = {
    val res = new Config.Res()
    copyRelevant(res, config.res)
    config.res = res
  }

  protected def copyRelevant(dst: Config.Res, src: Config.Res)

}

object ImportFileImage extends ImportFileType {
  def version = 30

  def copyRelevant(dst: Config.Res, src: Config.Res): Unit = {
    dst.image = src.image
    val tt = src.image.ttype
    if (tt == "sprite") {
      dst.atlas = src.atlas
      dst.sprite = src.sprite
    } else if (tt == "texture") {
      dst.texture = src.texture
    } else if (tt == "colorgrade") {
      dst.colorgrade = src.colorgrade
    } else if (tt == "") {
      throw new RuntimeException(s"Image has no texture type")
    } else {
      throw new RuntimeException(s"Unknown image type: $tt")
    }
  }
}

object ImportFileModel extends ImportFileType {
  def version = 41

  def copyRelevant(dst: Config.Res, src: Config.Res): Unit = {
    dst.animation = src.animation
    dst.mesh = src.mesh
    dst.model = src.model
  }
}

object ImportFilePcm extends ImportFileType {
  def version = 2

  def copyRelevant(dst: Config.Res, src: Config.Res): Unit = {
    dst.sound = src.sound
  }
}

object ImportFileAudio extends ImportFileType {
  def version = 1

  def copyRelevant(dst: Config.Res, src: Config.Res): Unit = {
    // No options
  }
}

object ImportFileFont extends ImportFileType {
  def version = 33

  def copyRelevant(dst: Config.Res, src: Config.Res): Unit = {
    dst.font = src.font
  }
}

object ImportFileShader extends ImportFileType {
  def version = 6

  def copyRelevant(dst: Config.Res, src: Config.Res): Unit = {
    dst.shader = src.shader
  }
}

object ImportFileLocale extends ImportFileType {
  def version = 8

  def copyRelevant(dst: Config.Res, src: Config.Res): Unit = {
    // No options
  }
}

object ImportFileEntity extends ImportFileType {
  def version = 5

  def copyRelevant(dst: Config.Res, src: Config.Res): Unit = {
    // No options
  }
}

/** Used when no importer is found */
object ImportFileNone extends ImportFileType {
  def version = 1

  def copyRelevant(dst: Config.Res, src: Config.Res): Unit = {
    // No options
  }
}
