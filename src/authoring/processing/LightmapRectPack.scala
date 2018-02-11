package tower.authoring.processing

object LightmapRectPack extends RectanglePacker {

  abstract class State
  case object Empty extends State
  case class Parent(left: Node, right: Node) extends State
  case class Leaf(index: Int) extends State

  class Node(val rect: Rectangle) {
    var state: State = Empty

    def insert(index: Int, size: Extents): Boolean = {
      state match {
        case Leaf(_) => false
        case Parent(left, right) => left.insert(index, size) || right.insert(index, size)
        case Empty =>
          if (rect.w < size.w || rect.h < size.h) {
            false
          } else if (rect.w == size.w && rect.h == size.h) {
            state = Leaf(index)
            true
          } else {
            val (left, right) = if ((rect.w - size.w) > (rect.h - size.h)) (
              Rectangle(rect.x         , rect.y, size.w         , rect.h),
              Rectangle(rect.x + size.w, rect.y, rect.w - size.w, rect.h),
            ) else (
              Rectangle(rect.x, rect.y         , rect.w, size.h         ),
              Rectangle(rect.x, rect.y + size.h, rect.w, rect.h - size.h),
            )
            val leftNode = new Node(left)
            state = Parent(leftNode, new Node(right))
            leftNode.insert(index, size)
          }
      }
    }
  }

  override def pack(container: Extents, sizes: Seq[Extents]): Option[Seq[Rectangle]] = {
    val root = new Node(Rectangle(0, 0, container.w, container.h))

    None
  }

}
