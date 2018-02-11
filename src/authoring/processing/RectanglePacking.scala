package tower.authoring.processing

case class Extents(w: Int, h: Int)
case class Rectangle(x: Int, y: Int, w: Int, h: Int)

trait RectanglePacker {

  def pack(container: Extents, sizes: Seq[Extents]): Option[Seq[Rectangle]]

}
