package game.system

import core._
import game.system.base._

object Entity {

  val Flag_Model = 0
  val Flag_PointLight = 1
  val Flag_PointLightReceiver = 2
  val Flag_Cullables = 3
  val Flag_AmbientProbes = 4
  val Flag_AmbientPointLight = 5
  val Flag_Children = 6
  val Flag_Parent = 7
  val Flag_Deleted = 8
  val Flag_Animator = 9

  val Flag_CablePart = 128
  val Flag_GroundPlate = 129
  val Flag_Tower = 130
  val Flag_Slots = 131
  val Flag_Cable = 132
  val Flag_GroundBlocker = 133
  val Flag_Enemy = 134

  val EmptyEntity = new EntityType(false, "BasicEntity", None)

}

/**
  * Entities are logical handles that exist between systems. In themselves they
  * contain minimal information.
  *
  * @param static If `true` the entity is considered immutable, meaning that
  *               it's position never changes and it's system-specific reference
  *               properties, eg. local position offsets, are never modified.
  */
class Entity(val static: Boolean, var name: String, val prototype: EntityType = Entity.EmptyEntity) {

  /** Index in the entity pool */
  val poolIndex = entitySystem.registerEntity(this)

  /**
    * Absolute position of the entity in the world.
    */
  var position: Vector3 = Vector3.Zero

  def setFlag(index: Int): Unit = {
    require(index >= 0 && index < 256, s"Flag index out of range: $index")

    if (index < 64) {
      flag0 |= 1L << index
    } else if (index < 128) {
      flag1 |= 1L << (index - 64)
    } else if (index < 192) {
      flag2 |= 1L << (index - 128)
    } else {
      flag3 |= 1L << (index - 192)
    }
  }

  def clearFlag(index: Int): Unit = {
    require(index >= 0 && index < 256, s"Flag index out of range: $index")

    if (index < 64) {
      flag0 &= ~(1L << index)
    } else if (index < 128) {
      flag1 &= ~(1L << (index - 64))
    } else if (index < 192) {
      flag2 &= ~(1L << (index - 128))
    } else {
      flag3 &= ~(1L << (index - 192))
    }
  }

  def hasFlag(index: Int): Boolean = {
    require(index >= 0 && index < 256, s"Flag index out of range: $index")

    if (index < 64) {
      (flag0 & (1L << index)) != 0
    } else if (index < 128) {
      (flag1 & (1L << (index - 64))) != 0
    } else if (index < 192) {
      (flag2 & (1L << (index - 128))) != 0
    } else {
      (flag3 & (1L << (index - 192))) != 0
    }
  }

  var flag0: Long = 0x0
  var flag1: Long = 0x0
  var flag2: Long = 0x0
  var flag3: Long = 0x0

  def delete(): Unit = {
    entitySystem.delete(this)
  }
}
