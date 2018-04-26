package game.system

import scala.collection.mutable.ArrayBuffer

/**
  * A set of entities. Does not internally check that entities are unique, but
  * contractually only contains unique entities.
  */
class EntitySet {

  /** Every entity in the set */
  val all = new ArrayBuffer[Entity]

  /** Entities which contain a specific flag */
  val flag = Array.fill(256)(new ArrayBuffer[Entity])

  /** Add an entity to a flag-specific list.
    *
    * Performance is relative to the number of set flags per entity,
    * unused flags are practically free.

    * @param entity Entity to add
    * @param mask Flag mask bits
    * @param base Index of the first mask bit
    */
  private def addByFlag(entity: Entity, mask: Long, base: Int): Unit = {
    var maskBits = mask
    while (maskBits != 0) {
      val index = java.lang.Long.numberOfTrailingZeros(maskBits)
      maskBits &= (maskBits - 1)

      flag(base + index) += entity
    }
  }

  /** Add an entity to the set.
    * Note: Never add an entity twice to a set. */
  def add(entity: Entity): Unit = {
    all += entity
    addByFlag(entity, entity.flag0, 0)
    addByFlag(entity, entity.flag1, 64)
    addByFlag(entity, entity.flag2, 128)
    addByFlag(entity, entity.flag3, 192)
  }

  /** Clear all the entities from the set. */
  def clear(): Unit = {
    all.clear()
    var ix = 0
    while (ix < flag.length) {
      flag(ix).clear()
      ix += 1
    }
  }

  def isEmpty: Boolean = all.isEmpty
  def nonEmpty: Boolean = all.nonEmpty

}

