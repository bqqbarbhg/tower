package res.intermediate

import util.Rectangle

/** A 2D bitmap image to be packed into an atlas */
class Sprite(val image: Image, val name: String) extends Resource {
  var imageBounds = new Rectangle(0, 0, image.width, image.height)
  var bounds = new Rectangle(0, 0, image.width, image.height)

  override def unload(): Unit = {}
}

