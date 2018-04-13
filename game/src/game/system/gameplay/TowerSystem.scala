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

  class Slot(val entity: Entity) {
    var locale: String = ""
    var isInput: Boolean = false
    var connection: Option[Slot] = None

    def detach(): Unit = {
      for (c <- connection) {
        c.connection = None
        connection = None
      }
    }
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

  /** Try to connect slots `a` and `b`, return whether it was succesful */
  def connectSlots(a: Slot, b: Slot): Boolean

}

object TowerSystemImpl {

  class Turret(val entity: Entity) extends CompactArrayPool.Element {
    var aimNode: Option[NodeInstance] = None
    var spinNode: Option[NodeInstance] = None

    var spin: Double = 0.0
  }

  val NoSlots = Array[Slot]()

  class SlotContainer(val entity: Entity) {
    var slots = new Array[Slot](0)

    def detachAll(): Unit = for (slot <- slots) slot.detach()
  }

}

final class TowerSystemImpl extends TowerSystem {

  val turrets = new CompactArrayPool[Turret]
  val entityToTurret = new mutable.HashMap[Entity, Turret]()
  val entityToSlots = new mutable.HashMap[Entity, SlotContainer]()

  def getSlotContainer(entity: Entity): SlotContainer = {
    entityToSlots.getOrElseUpdate(entity, {
      entity.setFlag(Flag_Slots)
      new SlotContainer(entity)
    })
  }

  override def addTurret(entity: Entity, turretComp: TurretTowerComponent): Unit = {
    val turret = new Turret(entity)
    val model = modelSystem.collectModels(entity).head
    turret.aimNode = model.findNode(turretComp.aimBone)
    turret.spinNode = model.findNode(turretComp.spinBone)
    entity.setFlag(Flag_Turret)
    entityToTurret(entity) = turret
    turrets.add(turret)

    val sc = getSlotContainer(entity)
    sc.slots :+= new Slot(entity) { locale = "slot.turret.aim"; isInput = true }
    sc.slots :+= new Slot(entity) { locale = "slot.turret.shoot"; isInput = true }
    sc.slots :+= new Slot(entity) { locale = "slot.turret.hits"; isInput = false }
  }

  override def entitiesDeleted(entities: EntitySet): Unit = {
    for (e <- entities.flag(Flag_Turret)) {
      turrets.remove(entityToTurret.remove(e).get)
      e.clearFlag(Flag_Turret)
    }
    for (e <- entities.flag(Flag_Slots)) {
      val slots = entityToSlots.remove(e).get
      slots.detachAll()
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
    if (entity.hasFlag(Flag_Slots)) {
      entityToSlots(entity).slots
    } else {
      NoSlots
    }
  }

  override def connectSlots(a: Slot, b: Slot): Boolean = {
    if (a == b) return false

    a.detach()
    b.detach()
    a.connection = Some(b)
    b.connection = Some(a)

    cableSystem.addCable(a, b)
    true
  }
}

