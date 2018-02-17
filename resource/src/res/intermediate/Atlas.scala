package res.intermediate

import util.Rectangle

import scala.collection.mutable.ArrayBuffer
import Atlas._

object Atlas {

  case class SpriteLocation(page: Int, rect: Rectangle)

}

class Atlas(val name: String) extends Resource {

  val sprites: ArrayBuffer[AssetFile] = ArrayBuffer[AssetFile]()
  var hasChanged: Boolean = false

  var locations: Array[SpriteLocation] = Array[SpriteLocation]()
  var pages: ArrayBuffer[Image] = ArrayBuffer[Image]()

  def unload(): Unit = {
    for (page <- pages) {
      page.unload()
    }
    pages.clear()
  }

}
