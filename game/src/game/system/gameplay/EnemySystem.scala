package game.system.gameplay

import core._
import game.system._
import game.system.Entity._
import game.component._
import game.system.gameplay._
import EnemySystem._
import EnemySystemImpl._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object EnemySystem {
  case class EnemyTarget(entity: Entity, position: Vector3, velocity: Vector3)
}

sealed trait EnemySystem extends EntityDeleteListener {

  /** Register an enemy to the system */
  def addEnemy(entity: Entity, component: EnemyComponent): Unit

  /** Query around an area. Guaranteed to return all enemies in a sphere
    * centered at `position` with radius `radius`. */
  def queryEnemiesAround(position: Vector3, radius: Double): Iterator[EnemyTarget]

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

  val NoPath = Vector[Vector2]()

  class Enemy(val entity: Entity, val component: EnemyComponent) extends CompactArrayPool.Element {
    var state: Int = StateUndefined
    var health: Double = component.health

    var path: Vector[Vector2] = NoPath
    var pathIndex: Int = 0

    var aimPos: Vector3 = Vector3.Zero
    var velocity: Vector3 = Vector3.Zero
  }

  class DelayedDamage(val entity: Entity, val damage: Double, var timeLeft: Double)

  val TargetReachDistance = 5.0
  val TargetReachDistanceSq = TargetReachDistance * TargetReachDistance


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

    enemy.path = pathfindSystem.findPath(entity.position.xz, Vector2.Zero)
  }

  override def queryEnemiesAround(position: Vector3, radius: Double): Iterator[EnemyTarget] = {
    // @Todo: Optimize this if needed...
    val radiusSq = radius * radius
    enemiesActive.iterator
      .filter(_.aimPos.distanceSquaredTo(position) <= radiusSq)
      .map(e => EnemyTarget(e.entity, e.aimPos, e.velocity))
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

      if (enemy.pathIndex < enemy.path.length) {
        val target = enemy.path(enemy.pathIndex)
        val pos = enemy.entity.position.xz
        val distSq = pos.distanceSquaredTo(target)
        if (distSq <= TargetReachDistanceSq) {
          enemy.pathIndex += 1
        } else {
          val dir = (target - pos) / math.sqrt(distSq)
          val dir3D = Vector3(dir.x, 0.0, dir.y)
          val vel = dir3D * 5.0

          enemy.velocity = vel
          enemy.entity.position += vel * dt
        }
      } else {
        enemy.velocity = Vector3.Zero
      }
    }
  }

  override def doDamageDelayed(entity: Entity, amount: Double, delay: Double): Unit = {
    delayedDamage += new DelayedDamage(entity, amount, delay)
  }

  override def doDamage(entity: Entity, amount: Double): Unit = {
    for (enemy <- entityToEnemy.get(entity)) {
      enemy.health -= amount

      val pos = enemy.entity.position.xz
      val range = Vector2(6.0, 6.0)
      val min = pos - range
      val max = pos + range
      pathfindSystem.increaseDynamicWeight(min, max, amount)

      if (enemy.health <= 0.0) {
        enemy.entity.delete()
      }
    }
  }

}

