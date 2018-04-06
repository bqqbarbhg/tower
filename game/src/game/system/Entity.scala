package game.system

import core._
import game.system.base._

object Entity {

  val Flag0_HasModel = 1L << 0
  val Flag0_HasPointLight = 1L << 1
  val Flag0_HasPointLightReceiver = 1L << 2
  val Flag0_HasCullables = 1L << 3
  val Flag0_HasAmbientProbes = 1L << 4
  val Flag0_HasAmbientPointLight = 1L << 5

  class EntityFlag0Iterator(val arr: Array[Entity], val mask: Long) extends Iterator[Entity] {
    private val arrLength = arr.length
    private var index: Int = nextIndex(0)
    private def nextIndex(start: Int): Int = {
      var ix = start
      val len = arrLength
      while (ix < len) {
        if ((arr(ix).flag0 & mask) != 0L) {
          return ix
        }
        ix += 1
      }
      ix
    }

    override def hasNext: Boolean = index < arrLength

    override def next(): Entity = {
      val ix = index
      val res = arr(ix)
      index = nextIndex(ix + 1)
      res
    }
  }

  def filterFlag0(arr: Array[Entity], mask: Long): EntityFlag0Iterator = new EntityFlag0Iterator(arr, mask)

}

/**
  * Entities are logical handles that exist between systems. In themselves they
  * contain minimal information.
  *
  * @param static If `true` the entity is considered immutable, meaning that
  *               it's position never changes and it's system-specific reference
  *               properties, eg. local position offsets, are never modified.
  */
class Entity(val static: Boolean) {

  /**
    * Absolute position of the entity in the world.
    */
  var position: Vector3 = Vector3.Zero

  var flag0: Long = 0x0
  var flag1: Long = 0x0
  var flag2: Long = 0x0
  var flag3: Long = 0x0

  def delete(): Unit = {
    entitySystem.delete(this)
  }
}
