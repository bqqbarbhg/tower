package game.system.gameplay

import core._
import game.system._
import game.system.base._
import game.system.rendering._
import game.system.Entity._
import game.component._
import EnemySystem._
import EnemySystemImpl._
import asset.EntityTypeAsset
import util.SparseGrid
import util.SparseGrid.CellPos

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object EnemySystem {
  case class EnemyTarget(entity: Entity, position: Vector3, velocity: Vector3)
}

sealed trait EnemySystem extends EntityDeleteListener {

  /** Register an enemy to the system */
  def addEnemy(entity: Entity, component: EnemyComponent): Unit

  /** Register an enemy blocker to the system */
  def addBlocker(entity: Entity, component: EnemyBlockerComponent): Unit

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

  val AiMove = 1
  val AiAttack = 2

  class Enemy(val entity: Entity, val component: EnemyComponent) extends CompactArrayPool.Element {
    val model = modelSystem.collectModels(entity).head
    val animator = animationSystem.addAnimator(entity, model)
    val walkLoop = animator.playLoop(0, component.moveAnim)

    val hitArea = Vector2(component.hitRadius, component.hitRadius)

    var enemyState: Int = StateUndefined
    var health: Double = component.health

    var path: Vector[Vector2] = NoPath
    var pathIndex: Int = 0

    var aimPos: Vector3 = Vector3.Zero
    var velocity: Vector3 = Vector3.Zero

    var attackTime: Double = 0.0
    var didAttack: Boolean = false
    var attackTarget: Entity = null

    var aiState: Int = AiMove

  }

  class Blocker(val entity: Entity, val component: EnemyBlockerComponent, val min: Vector2, val max: Vector2, val minPos: CellPos, val maxPos: CellPos, val next: Blocker) {
  }

  class DelayedDamage(val entity: Entity, val damage: Double, var timeLeft: Double)

  val TargetReachDistance = 5.0
  val TargetReachDistanceSq = TargetReachDistance * TargetReachDistance

  val BlockerGridSize = Vector2(8.0, 8.0)
  val BlockerGridOffset = Vector2(0.0, 0.0)

  class BlockerCell(val x: Int, val y: Int) {
    val blockers = new ArrayBuffer[Blocker]()
  }
}

final class EnemySystemImpl extends EnemySystem {

  val enemiesActive = new CompactArrayPool[Enemy]()

  val entityToEnemy = new mutable.HashMap[Entity, Enemy]()
  val entityToBlocker = new mutable.HashMap[Entity, Blocker]()

  val blockerGrid = new SparseGrid[BlockerCell](BlockerGridSize, BlockerGridOffset, (x, y) => new BlockerCell(x, y))

  val delayedDamage = new ArrayBuffer[DelayedDamage]()

  override def addEnemy(entity: Entity, component: EnemyComponent): Unit = {
    require(!entity.hasFlag(Flag_Enemy))

    val enemy = new Enemy(entity, component)
    enemy.enemyState = StateActive
    enemiesActive.add(enemy)
    entity.setFlag(Flag_Enemy)
    entityToEnemy(entity) = enemy

    enemy.path = pathfindSystem.findPath(entity.position.xz, Vector2.Zero)
  }

  override def addBlocker(entity: Entity, component: EnemyBlockerComponent): Unit = {
    val a = entity.transformPoint(component.min).xz
    val b = entity.transformPoint(component.max).xz

    val min = Vector2.min(a, b)
    val max = Vector2.max(a, b)

    val minCell = blockerGrid.getCellPosition(min)
    val maxCell = blockerGrid.getCellPosition(max)

    val next = if (entity.hasFlag(Flag_EnemyBlocker)) entityToBlocker(entity) else null
    val blocker = new Blocker(entity, component, min, max, minCell, maxCell, next)

    entityToBlocker(entity) = blocker

    for (cell <- blockerGrid.createRange(blocker.minPos, blocker.maxPos)) {
      cell.blockers += blocker
    }

    entity.setFlag(Flag_EnemyBlocker)
  }

  override def queryEnemiesAround(position: Vector3, radius: Double): Iterator[EnemyTarget] = {
    // @Todo: Optimize this if needed...
    val radiusSq = radius * radius
    enemiesActive.iterator
      .filter(_.aimPos.distanceSquaredTo(position) <= radiusSq)
      .map(e => EnemyTarget(e.entity, e.aimPos, e.velocity))
  }

  def removeBlocker(blocker: Blocker): Unit = {
    for (cell <- blockerGrid.getRange(blocker.minPos, blocker.maxPos)) {
      cell.blockers -= blocker
    }
  }

  override def entitiesDeleted(entities: EntitySet): Unit = {
    for (e <- entities.flag(Flag_Enemy)) {
      val enemy = entityToEnemy.remove(e).get

      if (enemy.enemyState == StateActive) {
        enemiesActive.remove(enemy)
      }

      e.clearFlag(Flag_Enemy)
    }

    for (e <- entities.flag(Flag_EnemyBlocker)) {
      var blocker = entityToBlocker(e)

      do {

        removeBlocker(blocker)

        blocker = blocker.next
      } while (blocker != null)

      e.clearFlag(Flag_EnemyBlocker)
    }
  }

  def queryBlocker(enemy: Enemy): Option[Blocker] = {
    val queryPos = enemy.entity.position.xz
    val queryMin = queryPos - enemy.hitArea
    val queryMax = queryPos + enemy.hitArea
    for {
      cell <- blockerGrid.getIntersecting(queryMin, queryMax)
      blocker <- cell.blockers
    } {
      if (!(queryMax.x <= blocker.min.x || queryMax.y <= blocker.min.y || queryMin.x >= blocker.max.x || queryMin.y >= blocker.max.y)) {
        return Some(blocker)
      }
    }

    None
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
      enemy.velocity = Vector3.Zero

      if (enemy.aiState == AiMove) {
        queryBlocker(enemy) match {
          case Some(blocker) =>
            enemy.aiState = AiAttack
            enemy.didAttack = false
            enemy.attackTime = 0.0
            enemy.attackTarget = blocker.entity

            enemy.animator.playOnce(1, enemy.component.attackAnim, 0.2, 0.3)

          case None =>
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
      } else if (enemy.aiState == AiAttack) {

        enemy.attackTime += dt

        if (enemy.attackTime >= enemy.component.meleeHitTime && !enemy.didAttack) {
          enemy.didAttack = true
          towerSystem.doDamage(enemy.attackTarget, enemy.component.meleeDamage)
        }

        if (enemy.attackTime >= enemy.component.meleeDuration) {
          enemy.aiState = AiMove
        }

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

