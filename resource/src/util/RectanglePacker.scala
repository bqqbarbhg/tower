package util

case class Extents(w: Int, h: Int)
case class Rectangle(x: Int, y: Int, w: Int, h: Int)

object RectanglePacker {

  /** Get a rectangle packer by name */
  def get(name: String): Option[RectanglePacker] = name match {
    case "lightmap" => Some(LightmapRectanglePacker)
    case other => None
  }

}

trait RectanglePacker {
  def pack(container: Extents, sizes: Map[Int, Extents]): Map[Int, Rectangle]
}
