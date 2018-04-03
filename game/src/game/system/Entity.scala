package game.system

import core._
import game.system.base._

object Entity {

  val Flag0_HasModel = 1L << 0
  val Flag0_HasPointLight = 1L << 1
  val Flag0_HasPointLightReceiver = 1L << 2
  val Flag0_HasCullables = 1L << 3

}

class Entity {

  /**
    * Logical position of the entity in the world.
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
