package game.system.gameplay

import java.nio.ByteBuffer

import core._
import util.BufferUtils._
import asset.EntityTypeAsset
import game.system._
import game.system.gameplay.TowerSystem.Slot
import game.system.base._
import game.system.Entity._
import SaveStateSystemImpl._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait SaveStateSystem extends EntityDeleteListener {

  /** Save a tower */
  def registerSavedTower(entity: Entity): Unit

  /** Remove a tower from the save */
  def unregisterSavedTower(entity: Entity): Unit

  /** Save a cable */
  def registerConnection(a: Slot, b: Slot): Unit

  /** Remove a cable from the save */
  def unregisterConnection(a: Slot): Unit

  /** Restore a previous save state */
  def recreateMissingEntities(): Unit

  /** Write the saved state into a buffer */
  def save(buffer: ByteBuffer): Unit

  /** Read the saved state from a uffer */
  def load(buffer: ByteBuffer): Unit

}

object SaveStateSystemImpl {

  val NoCables = Array[SavedCable]()

  class SavedTower(val entityType: EntityTypeAsset, val position: Vector3, val rotation: Quaternion) extends CompactArrayPool.Element {
    var instance: Option[Entity] = None
    var cables: Array[SavedCable] = NoCables
  }

  class SavedCable(val towerA: SavedTower, val idA: Identifier, val towerB: SavedTower, val idB: Identifier) extends CompactArrayPool.Element {
    var slotA: Option[Slot] = None
    var slotB: Option[Slot] = None

    def isConnected: Boolean = slotA.exists(_.connection.nonEmpty) && slotB.exists(_.connection.nonEmpty)
  }

}

final class SaveStateSystemImpl extends SaveStateSystem {

  var serialCounter = 0

  val savedTowers = new CompactArrayPool[SavedTower]()
  val entityToTower = new mutable.HashMap[Entity, SavedTower]()

  val savedCables = new CompactArrayPool[SavedCable]()
  val slotToCable = new mutable.HashMap[Slot, SavedCable]()

  override def registerSavedTower(entity: Entity): Unit = {
    if (entity.hasFlag(Flag_SaveState)) return

    for (asset <- entity.prototype.asset) {
      val tower = new SavedTower(asset, entity.position, entity.rotation)
      tower.instance = Some(entity)
      savedTowers.add(tower)
      entityToTower(entity) = tower
      entity.setFlag(Flag_SaveState)
    }
  }

  override def unregisterSavedTower(entity: Entity): Unit = {
    if (!entity.hasFlag(Flag_SaveState)) return

    val tower = entityToTower.remove(entity).get
    savedTowers.remove(tower)
    entity.clearFlag(Flag_SaveState)

    for (cable <- tower.cables; slot <- cable.slotA) {
      unregisterConnection(slot)
    }
  }

  override def registerConnection(a: Slot, b: Slot): Unit = {
    unregisterConnection(a)
    unregisterConnection(b)

    val entityA = a.entity
    val entityB = b.entity

    if (entityA.hasFlag(Flag_SaveState) && entityB.hasFlag(Flag_SaveState)) {
      val towerA = entityToTower(entityA)
      val towerB = entityToTower(entityB)

      val cable = new SavedCable(towerA, a.info.id, towerB, b.info.id)
      towerA.cables :+= cable
      towerB.cables :+= cable
      cable.slotA = Some(a)
      cable.slotB = Some(b)

      slotToCable(a) = cable
      slotToCable(b) = cable
      savedCables.add(cable)
    }
  }

  override def unregisterConnection(a: Slot): Unit = {
    for (cable <- slotToCable.get(a)) {
      savedCables.remove(cable)

      cable.towerA.cables = cable.towerA.cables.filterNot(_ eq cable)
      cable.towerB.cables = cable.towerB.cables.filterNot(_ eq cable)

      for (slot <- cable.slotA) slotToCable.remove(slot)
      for (slot <- cable.slotB) slotToCable.remove(slot)
    }
  }

  override def recreateMissingEntities(): Unit = {

    // First create towers
    for (tower <- savedTowers if tower.instance.isEmpty) {
      val entity = entitySystem.create(tower.entityType.get, tower.position, tower.rotation)
      tower.instance = Some(entity)
      entityToTower(entity) = tower
      entity.setFlag(Flag_SaveState)
    }

    // Then create missing cables
    for (cable <- savedCables if !cable.isConnected) {

      for {
        entityA <- cable.towerA.instance
        entityB <- cable.towerB.instance
      } {
        val slotsA = towerSystem.getSlots(entityA)
        val slotsB = towerSystem.getSlots(entityB)

        for {
          slotA <- slotsA.find(_.info.id == cable.idA) if slotA.connection.isEmpty
          slotB <- slotsB.find(_.info.id == cable.idB) if slotB.connection.isEmpty
        } {

          towerSystem.connectSlots(slotA, slotB)
          cable.slotA = Some(slotA)
          cable.slotB = Some(slotB)
          slotToCable(slotA) = cable
          slotToCable(slotB) = cable
        }
      }
    }

  }

  override def entitiesDeleted(entities: EntitySet): Unit = {
    for (entity <- entities.flag(Flag_SaveState)) {
      val tower = entityToTower.remove(entity).get
      tower.instance = None
      entity.clearFlag(Flag_SaveState)
    }
  }

  override def save(buffer: ByteBuffer): Unit = {
    val types = new mutable.HashMap[Identifier, Int]()

    for (tower <- savedTowers) {
      types.getOrElseUpdate(tower.entityType.name, types.size)
    }

    val linearTypes = types.toSeq.sortBy(_._2).map(_._1)

    val Version = 2
    buffer.putMagic("s2ts")
    buffer.putInt(Version)

    buffer.putInt(linearTypes.length)

    for (typ <- linearTypes) {
      buffer.putIdentifier(typ.toString)
    }

    val towerToIndex = new mutable.HashMap[SavedTower, Int]()

    buffer.putInt(savedTowers.length)

    for ((tower, index) <- savedTowers.zipWithIndex) {
      val typeIndex = types(tower.entityType.name)
      buffer.putInt(typeIndex)
      buffer.putVector3(tower.position)
      buffer.putQuaternion(tower.rotation)

      towerToIndex(tower) = index
    }

    val validCables = savedCables.filter(c => {
      towerToIndex.contains(c.towerA) && towerToIndex.contains(c.towerB)
    })

    buffer.putInt(validCables.length)

    for (cable <- validCables) {
      buffer.putInt(towerToIndex(cable.towerA))
      buffer.putInt(towerToIndex(cable.towerB))
      buffer.putIdentifier(cable.idA.toString)
      buffer.putIdentifier(cable.idB.toString)
    }

    buffer.putMagic("E.ts")
  }

  override def load(buffer: ByteBuffer): Unit = {

    savedTowers.clear()
    entityToTower.clear()

    savedCables.clear()
    slotToCable.clear()

    val MinVersion = 2
    val MaxVersion = 2
    buffer.verifyMagic("s2ts")
    val version = buffer.getVersion(MaxVersion, MinVersion)

    val numTypes = buffer.getInt()

    val types = Array.fill(numTypes) {
      EntityTypeAsset(buffer.getIdentifier())
    }

    val numTowers = buffer.getInt()

    for (i <- 0 until numTowers) {
      val typeIndex = buffer.getInt()
      val typ = types(typeIndex)

      val pos = buffer.getVector3()
      val rot = buffer.getQuaternion()

      val tower = new SavedTower(typ, pos, rot)
      savedTowers.add(tower)
    }

    val numCables = buffer.getInt()

    for (i <- 0 until numCables) {
      val indexA = buffer.getInt()
      val indexB = buffer.getInt()
      val idA = buffer.getIdentifier()
      val idB = buffer.getIdentifier()

      // Note: This is safe because savedTowers is cleared and nothing is removed
      // between adding the towers and referencing them here by index.
      val towerA = savedTowers(indexA)
      val towerB = savedTowers(indexB)

      savedCables.add(new SavedCable(towerA, idA, towerB, idB))
    }

    buffer.verifyMagic("E.ts")
  }

}

