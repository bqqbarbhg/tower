package core

import core.ArrayPool.CompactIterator

import scala.reflect.ClassTag

object ArrayPool {

  final class CompactIterator[T >: Null : ClassTag](val pool: ArrayPool[T]) extends Iterator[T] {
    var position: Int = 0
    var numIterated: Int = 0

    override def hasNext: Boolean = numIterated < pool.size

    override def next(): T = {
      var localPos = position
      while (pool.sparseData(localPos) == null)
        localPos += 1
      position = localPos + 1
      numIterated += 1
      pool.sparseData(localPos)
    }

  }

}

final class ArrayPool[T >: Null : ClassTag] extends Iterable[T] {
  var sparseData: Array[T] = new Array[T](0)
  private var sparseDataSize: Int = 0

  private var freeList: Array[Int] = new Array[Int](0)
  private var freeListSize = 0

  override def iterator: Iterator[T] = new CompactIterator[T](this)
  override def size: Int = sparseDataSize

  def add(t: T): Int = {
    if (sparseDataSize >= sparseData.length) {
      val newLength = math.max(sparseData.length * 2, 16)
      val oldData = sparseData
      sparseData = new Array[T](newLength)
      java.lang.System.arraycopy(oldData, 0, sparseData, 0, oldData.length)
    }

    val index = if (freeListSize > 0) {
      freeListSize -= 1
      val freeListIx = freeListSize
      freeList(freeListIx)
    } else {
      sparseDataSize
    }

    sparseData(index) = t

    sparseDataSize += 1

    index
  }

  def remove(index: Int): Unit = {
    sparseData(index) = null
    sparseDataSize -= 1

    if (freeListSize >= freeList.length) {
      val newLength = math.max(freeList.length * 2, 16)
      val oldData = freeList
      freeList = new Array[Int](newLength)
      java.lang.System.arraycopy(oldData, 0, freeList, 0, oldData.length)
    }

    freeList(freeListSize) = index
    freeListSize += 1
  }

  def clear(): Unit = {
    var ix = 0
    while (ix < sparseData.length) {
      sparseData(ix) = null
      ix += 1
    }
    sparseDataSize = 0
    freeListSize = 0
  }

}

