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

  class Slot(val entity: Entity, val isInput: Boolean, val locale: String) {
    var connection: Option[Slot] = None

    def detach(): Unit = {
      for (c <- connection) {
        cableSystem.removeCable(this, c)
        connectionSystem.removeConnection(this, c)

        c.connection = None
        connection = None
      }
    }
  }

}

sealed trait TowerSystem extends EntityDeleteListener {

  /** Add a tower component to the system */
  def addComponent(entity: Entity, comp: Component): Unit

  /** Update all the towers */
  def update(dt: Double): Unit

  /** Update visible entities for rendering */
  def updateVisible(visible: EntitySet): Unit

  /** Retrieve connectable slots from an entity */
  def getSlots(entity: Entity): Seq[Slot]

  /** Try to connect slots `a` and `b`, return whether it was succesful */
  def connectSlots(a: Slot, b: Slot): Boolean

  /** Add or modify a radar target */
  def updateRadarTarget(entity: Entity, position: Vector3): Unit

}

object TowerSystemImpl {

  abstract class Tower extends CompactArrayPool.Element {
    var next: Tower = null

    def slots: Array[Slot]
    def update(dt: Double): Unit
    def updateVisible(): Unit
  }

  class Turret(val entity: Entity, val component: TurretTowerComponent) extends Tower {
    val model = modelSystem.collectModels(entity).head

    var aimNode: Option[NodeInstance] = model.findNode(component.aimBone)
    var spinNode: Option[NodeInstance] = model.findNode(component.spinBone)

    var spin: Double = 0.0

    val slotTargetIn = new Slot(entity, true, "slot.turret.targetIn")
    val slotTargetOut = new Slot(entity, false, "slot.turret.targetOut")

    override val slots: Array[Slot] = Array(
      slotTargetIn,
      slotTargetOut,
    )

    def update(dt: Double): Unit = {
      spin += dt * 5.0
    }

    override def updateVisible(): Unit = {
      for (node <- spinNode) {
        node.localTransform = Matrix43.rotateY(spin)
      }
    }

  }

  class RotatingRadar(val entity: Entity, val component: RotatingRadarComponent) extends Tower {
    val model = modelSystem.collectModels(entity).head

    var rotateNode: Option[NodeInstance] = model.findNode(component.rotateBone)

    var angle: Double = 0.0

    val slotTargetOut = new Slot(entity, false, "slot.radar.targetOut")

    override val slots: Array[Slot] = Array(
      slotTargetOut,
    )

    def update(dt: Double): Unit = {
      angle += dt * 2.0
    }

    override def updateVisible(): Unit = {
      for (node <- rotateNode) {
        node.localTransform = Matrix43.rotateZ(angle)
      }
    }
  }

  val NoSlots = Array[Slot]()

  class SlotContainer(val entity: Entity) {
    var slots = new Array[Slot](0)

    def detachAll(): Unit = for (slot <- slots) slot.detach()
  }

}

final class TowerSystemImpl extends TowerSystem {

  val towers = new CompactArrayPool[Tower]

  val entityToTower = new mutable.HashMap[Entity, Tower]()
  val entityToSlots = new mutable.HashMap[Entity, SlotContainer]()

  def getSlotContainer(entity: Entity): SlotContainer = {
    entityToSlots.getOrElseUpdate(entity, {
      entity.setFlag(Flag_Slots)
      new SlotContainer(entity)
    })
  }

  override def addComponent(entity: Entity, comp: Component): Unit = {
    val tower = comp match {
      case c: TurretTowerComponent => new Turret(entity, c)
      case c: RotatingRadarComponent => new RotatingRadar(entity, c)
    }

    if (entity.hasFlag(Flag_Tower)) {
      tower.next = entityToTower(entity)
    }

    val slots = getSlotContainer(entity)
    entity.setFlag(Flag_Tower)
    slots.slots ++= tower.slots
    entityToTower(entity) = tower
    towers.add(tower)
  }

  override def entitiesDeleted(entities: EntitySet): Unit = {
    for (e <- entities.flag(Flag_Tower)) {
      var tower = entityToTower(e)
      do {
        towers.remove(tower)
        tower = tower.next
      } while (tower != null)
      e.clearFlag(Flag_Tower)
    }
    for (e <- entities.flag(Flag_Slots)) {
      val slots = entityToSlots.remove(e).get
      slots.detachAll()
    }
  }

  override def update(dt: Double): Unit = {
    for (tower <- towers) {
      tower.update(dt)
    }
  }

  override def updateVisible(visible: EntitySet): Unit = {
    for (e <- visible.flag(Flag_Tower)) {
      var tower = entityToTower(e)
      do {
        tower.updateVisible()
        tower = tower.next
      } while (tower != null)
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
    if (a.isInput && b.isInput) return false
    if (!a.isInput && !b.isInput) return false

    val (src, dst) = if (a.isInput) (a, b) else (b, a)

    src.detach()
    dst.detach()

    src.connection = Some(dst)
    dst.connection = Some(src)

    val cable = cableSystem.addCable(src, dst)
    connectionSystem.addConnection(src, dst, cable)

    true
  }

  override def updateRadarTarget(entity: Entity, position: Vector3): Unit = {

  }
}

