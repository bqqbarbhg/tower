package res.process

import res.intermediate._
import util.Rectangle

/**
  * Processes images into sprites. The sprites take ownership of the images they
  * are created from and the images should not be unloaded directly afterwards.
  */
object ProcessSprite {

  private def processSpriteImpl(sprite: Sprite, config: Config.Res.Sprite): Unit = {
    if (config.crop) {
      CropSprite.cropSprite(sprite, config)
    }

    sprite.wrapX = config.wrapX
    sprite.wrapY = config.wrapY
  }

  def processSprite(image: Image, name: String, config: Config.Res.Sprite): Seq[Sprite] = {
    val sprites: Seq[Sprite] = if (config.animation) {
      val frameW = image.width / config.framesX
      val frameH = image.height / config.framesY
      var ix = 0
      val frames = for {
        y <- 0 until config.framesY
        x <- 0 until config.framesX
      } yield {
        val frame = new Sprite(image, name + "." + ix)
        frame.imageBounds = Rectangle(image.width * x / config.framesX, image.height * y / config.framesY, frameW, frameH)
        frame.bounds = frame.imageBounds
        ix += 1
        frame
      }

      frames.toSeq
    } else {
      Array(new Sprite(image, name))
    }

    for (sprite <- sprites) {
      processSpriteImpl(sprite, config)
    }

    sprites
  }

}


