package game.system.gameplay

import game.component._
import game.system._
import game.system.Entity._
import TowerSystemImpl._
import core.{CompactArrayPool, Matrix43}
import game.system.rendering._
import game.system.rendering.ModelSystem._

import scala.collection.mutable

sealed trait TowerSystem extends EntityDeleteListener {

  /** Add a turret to the system */
  def addTurret(entity: Entity, turretComp: TurretTowerComponent): Unit

  /** Update all the towers */
  def update(dt: Double): Unit

  /** Update visible entities for rendering */
  def updateVisible(visible: EntitySet): Unit

}

object TowerSystemImpl {

  class Turret(val entity: Entity) extends CompactArrayPool.Element {
    var aimNode: Option[NodeInstance] = None
    var spinNode: Option[NodeInstance] = None

    var spin: Double = 0.0
  }

}

final class TowerSystemImpl extends TowerSystem {

  val turrets = new CompactArrayPool[Turret]
  val entityToTurret = new mutable.HashMap[Entity, Turret]()

  override def addTurret(entity: Entity, turretComp: TurretTowerComponent): Unit = {
    val turret = new Turret(entity)
    val model = modelSystem.collectModels(entity).head
    turret.aimNode = model.findNode(turretComp.aimBone)
    turret.spinNode = model.findNode(turretComp.spinBone)
    entity.setFlag(Flag_Turret)
    entityToTurret(entity) = turret
    turrets.add(turret)
  }

  override def entitiesDeleted(entities: EntitySet): Unit = {
    for (e <- entities.flag(Flag_Turret)) {
      turrets.remove(entityToTurret.remove(e).get)
      e.clearFlag(Flag_Turret)
    }
  }

  override def update(dt: Double): Unit = {
    for (turret <- turrets) {
      turret.spin += dt * 5.0
    }
  }

  override def updateVisible(visible: EntitySet): Unit = {
    for (e <- visible.flag(Flag_Turret)) {
      val turret = entityToTurret(e)
      for (spin <- turret.spinNode) {
        spin.localTransform = Matrix43.rotateY(turret.spin)
      }
    }
  }
}

