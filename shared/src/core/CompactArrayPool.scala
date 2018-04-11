package core

import CompactArrayPool._

import scala.reflect.ClassTag

object CompactArrayPool {

  trait ElementBase {
    def compactPoolIndex: Int
    def compactPoolIndexChanged(newIndex: Int): Unit
    def compactPoolAdded(newIndex: Int): Unit = compactPoolIndexChanged(newIndex)
    def compactPoolRemoved(): Unit = compactPoolIndexChanged(-1)
  }

  trait Element extends ElementBase {
    private var _compactPoolIndex: Int = -1
    override def compactPoolIndex: Int = _compactPoolIndex
    override def compactPoolIndexChanged(newIndex: Int): Unit = _compactPoolIndex = newIndex
  }

}

class CompactArrayPool[A >: Null <: ElementBase : ClassTag] extends Seq[A] {
  var num: Int = 0
  var arr: Array[A] = new Array[A](0)

  override def apply(idx: Int): A = {
    require(idx >= 0 && idx < num)
    arr(idx)
  }
  override def length: Int = num
  override def iterator: Iterator[A] = arr.iterator.take(num)

  def add(element: A): Unit = {
    require(element.compactPoolIndex < 0)

    if (num >= arr.length) {
      val newLength = math.max(arr.length * 2, 16)
      val old = arr
      arr = new Array[A](newLength)
      java.lang.System.arraycopy(old, 0, arr, 0, old.length)
    }

    arr(num) = element
    element.compactPoolAdded(num)
    num += 1
  }

  def remove(element: A): Unit = {
    remove(element.compactPoolIndex)
  }

  def remove(index: Int): Unit = {
    require(index >= 0 && index < num)
    require(num > 0)

    if (index != num - 1) {
      val elem = arr(num - 1)
      arr(index) = elem
      elem.compactPoolIndexChanged(index)
    }

    arr(num) = null
    num -= 1
  }

}

