package game.system.gameplay

import game.system._
import game.system.Entity._
import EnemySystemImpl._
import core.{CompactArrayPool, Vector3}
import game.component._
import game.system.gameplay._

import scala.collection.mutable

sealed trait EnemySystem extends EntityDeleteListener {

  /** Register an enemy to the system */
  def addEnemy(entity: Entity, component: EnemyComponent): Unit

  /** Query around an area. Guaranteed to return all enemies in a sphere
    * centered at `position` with radius `radius`. */
  def queryEnemiesAround(position: Vector3, radius: Double): Iterator[Entity]

}

object EnemySystemImpl {

  class Enemy(val entity: Entity, val component: EnemyComponent) extends CompactArrayPool.Element {
  }

}

final class EnemySystemImpl extends EnemySystem {

  val enemies = new CompactArrayPool[Enemy]()
  val entityToEnemy = new mutable.HashMap[Entity, Enemy]()

  override def addEnemy(entity: Entity, component: EnemyComponent): Unit = {
    require(!entity.hasFlag(Flag_Enemy))

    val enemy = new Enemy(entity, component)
    enemies.add(enemy)
    entity.setFlag(Flag_Enemy)
    entityToEnemy(entity) = enemy
  }

  override def queryEnemiesAround(position: Vector3, radius: Double): Iterator[Entity] = {
    // @Todo: Optimize this if needed...
    val radiusSq = radius * radius
    enemies.iterator.filter(_.entity.position.distanceSquaredTo(position) <= radiusSq).map(_.entity)
  }

  override def entitiesDeleted(entities: EntitySet): Unit = {
    for (e <- entities.flag(Flag_Enemy)) {
      val enemy = entityToEnemy.remove(e).get
      enemies.remove(enemy)
      e.clearFlag(Flag_Enemy)
    }
  }

}
