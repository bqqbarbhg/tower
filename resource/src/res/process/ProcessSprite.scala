package res.process

import res.intermediate._

/**
  * Processes images into sprites. The sprites take ownership of the images they
  * are created from and the images should not be unloaded directly afterwards.
  */
object ProcessSprite {

  def processSprite(image: Image, config: Config.Res.Sprite): Sprite = {
    val sprite = new Sprite(image)

    if (config.crop) {
      CropSprite.cropSprite(sprite)
    }

    sprite
  }

}


