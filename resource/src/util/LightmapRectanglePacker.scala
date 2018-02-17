package util

import collection.mutable
import collection.mutable.ArrayBuffer

/**
  * Rectangle packer based on a KD-tree.
  * Based on: http://blackpawn.com/texts/lightmaps/
  */
object LightmapRectanglePacker extends RectanglePacker {
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

    def collect(results: ArrayBuffer[Rectangle]): Unit = {
      state match {
        case Leaf(index) => results(index) = rect
        case Parent(left, right) =>
          left.collect(results)
          right.collect(results)
        case Empty => // Nop
      }
    }

    def collect(results: mutable.HashMap[Int, Rectangle]): Unit = {
      state match {
        case Leaf(index) => results(index) = rect
        case Parent(left, right) =>
          left.collect(results)
          right.collect(results)
        case Empty => // Nop
      }
    }
  }

  private def sortSizes(sizes: Seq[(Int, Extents)]): Iterable[(Int, Extents)] = {
    // Sort by larger extent, but break ties with the smaller one
    sizes.sortBy(pair => math.max(pair._2.w, pair._2.h) * 32 + math.min(pair._2.w, pair._2.h))
  }

  override def pack(container: Extents, sizes: Map[Int, Extents]): Map[Int, Rectangle] = {
    val root = new Node(Rectangle(0, 0, container.w, container.h))

    for ((index, size) <- sortSizes(sizes.toSeq)) {
      root.insert(index, size)
    }

    val result = new mutable.HashMap[Int, Rectangle]()
    root.collect(result)
    result.toMap
  }
}
