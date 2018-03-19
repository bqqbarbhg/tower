package res.process

import core._
import res.intermediate._
import util.Rectangle

/**
  * Crop the invisible area around a sprite.
  */
object CropSprite {

  /** Crop the sprite in-place */
  def cropSprite(sprite: Sprite, config: Config.Res.Sprite): Unit = {
    val img = sprite.image

    val ib = sprite.imageBounds
    val rows = ib.y until (ib.y + ib.h)
    val cols = ib.x until (ib.x + ib.w)

    def pixelVisible(x: Int, y: Int) = {
      val pixel = img.getPixel(x, y)
      if (config.cropByAllChannels)
        pixel.r >= 0.0001 || pixel.g >= 0.0001 || pixel.b >= 0.0001 || pixel.a >= 0.0001
      else
        pixel.a >= 0.0001
    }
    def colVisible(x: Int) = rows.exists(pixelVisible(x, _))
    def rowVisible(y: Int) = cols.exists(pixelVisible(_, y))

    val minX = cols.find(colVisible)
    val maxX = cols.reverse.find(colVisible)
    val minY = rows.find(rowVisible)
    val maxY = rows.reverse.find(rowVisible)

    (minX, maxX, minY, maxY) match {
      case (Some(x0), Some(x1), Some(y0), Some(y1)) =>
        // Convert inclusive min/max to exclusive origin/size
        sprite.bounds = new Rectangle(x0, y0, x1 - x0 + 1, y1 - y0 + 1)
      case (None, None, None, None) =>
        // The whole image is empty, just crop to 1x1 rect for now
        sprite.bounds = new Rectangle(0, 0, 1, 1)
      case _ =>
        // If any of the values are `None` it should mean the image is empty,
        // which in turn should mean they all are `None`
        unreachable("Crop bounds are incoherent")
    }
  }

}

