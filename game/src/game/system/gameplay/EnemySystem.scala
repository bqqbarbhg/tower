package game.system.gameplay

import game.system._
import game.system.Entity._
import EnemySystemImpl._
import core.{CompactArrayPool, Vector3}
import game.component._
import game.system.gameplay._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait EnemySystem extends EntityDeleteListener {

  /** Register an enemy to the system */
  def addEnemy(entity: Entity, component: EnemyComponent): Unit

  /** Query around an area. Guaranteed to return all enemies in a sphere
    * centered at `position` with radius `radius`. */
  def queryEnemiesAround(position: Vector3, radius: Double): Iterator[(Entity, Vector3)]

  /** Update all the active enemies */
  def update(dt: Double): Unit

  /** Do damage to an enemy */
  def doDamage(entity: Entity, amount: Double): Unit

  /** Do damage to an enemy after some time */
  def doDamageDelayed(entity: Entity, amount: Double, delay: Double): Unit

}

object EnemySystemImpl {

  val StateUndefined = 0
  val StateActive = 1

  class Enemy(val entity: Entity, val component: EnemyComponent) extends CompactArrayPool.Element {
    var state: Int = StateUndefined
    var health: Double = component.health

    var aimPos: Vector3 = Vector3.Zero
  }

  class DelayedDamage(val entity: Entity, val damage: Double, var timeLeft: Double)

}

final class EnemySystemImpl extends EnemySystem {

  val enemiesActive = new CompactArrayPool[Enemy]()

  val entityToEnemy = new mutable.HashMap[Entity, Enemy]()
  val delayedDamage = new ArrayBuffer[DelayedDamage]()

  override def addEnemy(entity: Entity, component: EnemyComponent): Unit = {
    require(!entity.hasFlag(Flag_Enemy))

    val enemy = new Enemy(entity, component)
    enemy.state = StateActive
    enemiesActive.add(enemy)
    entity.setFlag(Flag_Enemy)
    entityToEnemy(entity) = enemy
  }

  override def queryEnemiesAround(position: Vector3, radius: Double): Iterator[(Entity, Vector3)] = {
    // @Todo: Optimize this if needed...
    val radiusSq = radius * radius
    enemiesActive.iterator
      .filter(_.aimPos.distanceSquaredTo(position) <= radiusSq)
      .map(e => (e.entity, e.aimPos))
  }

  override def entitiesDeleted(entities: EntitySet): Unit = {
    for (e <- entities.flag(Flag_Enemy)) {
      val enemy = entityToEnemy.remove(e).get

      if (enemy.state == StateActive) {
        enemiesActive.remove(enemy)
      }

      e.clearFlag(Flag_Enemy)
    }
  }

  override def update(dt: Double): Unit = {
    {
      var ix = 0
      while (ix < delayedDamage.length) {
        val damage = delayedDamage(ix)
        damage.timeLeft -= dt
        if (damage.timeLeft <= 0.0) {
          doDamage(damage.entity, damage.damage)
          delayedDamage(ix) = delayedDamage.last
          delayedDamage.trimEnd(1)
        } else {
          ix += 1
        }
      }
    }

    for (enemy <- enemiesActive) {
      enemy.aimPos = enemy.entity.transformPoint(enemy.component.aimPosition)
    }
  }

  override def doDamageDelayed(entity: Entity, amount: Double, delay: Double): Unit = {
    delayedDamage += new DelayedDamage(entity, amount, delay)
  }

  override def doDamage(entity: Entity, amount: Double): Unit = {
    for (enemy <- entityToEnemy.get(entity)) {
      enemy.health -= amount

      if (enemy.health <= 0.0) {
        enemy.entity.delete()
      }
    }



  }

}

