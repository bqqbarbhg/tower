package game.system.gameplay

import core._
import game.component._
import game.system._
import game.system.Entity._
import game.system.rendering._
import game.system.rendering.ModelSystem._
import TowerSystem._
import TowerSystemImpl._

import scala.collection.mutable

object TowerSystem {

  class Slot {
    var locale: String = ""
    var isInput: Boolean = false
  }

}

sealed trait TowerSystem extends EntityDeleteListener {

  /** Add a turret to the system */
  def addTurret(entity: Entity, turretComp: TurretTowerComponent): Unit

  /** Update all the towers */
  def update(dt: Double): Unit

  /** Update visible entities for rendering */
  def updateVisible(visible: EntitySet): Unit

  /** Retrieve connectable slots from an entity */
  def getSlots(entity: Entity): Seq[Slot]

}

object TowerSystemImpl {

  class Turret(val entity: Entity) extends CompactArrayPool.Element {
    var aimNode: Option[NodeInstance] = None
    var spinNode: Option[NodeInstance] = None

    var spin: Double = 0.0
  }

  val NoSlots = Array[Slot]()

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

  override def getSlots(entity: Entity): Seq[Slot] = {
    if (entity.hasFlag(Flag_Turret)) {
      var slots = Array[Slot]()
      slots :+= new Slot() { locale = "slot.turret.aim"; isInput = true }
      slots :+= new Slot() { locale = "slot.turret.shoot"; isInput = true }
      slots :+= new Slot() { locale = "slot.turret.hits"; isInput = false }
      slots
    } else {
      NoSlots
    }
  }
}

