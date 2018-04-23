package game.system.gameplay

import java.nio.ByteBuffer

import core._
import util.BufferUtils._
import asset.EntityTypeAsset
import game.system._
import game.system.gameplay.SaveStateSystemImpl.SavedTower
import game.system.gameplay.TowerSystem.Slot
import game.system.base._
import game.system.Entity._

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
  def unregisterConnection(a: Slot, b: Slot): Unit

  /** Restore a previous save state */
  def recreateMissingEntities(): Unit

  /** Write the saved state into a buffer */
  def save(buffer: ByteBuffer): Unit

  /** Read the saved state from a uffer */
  def load(buffer: ByteBuffer): Unit

}

object SaveStateSystemImpl {

  class SavedTower(val entityType: EntityTypeAsset, val position: Vector3, val rotation: Quaternion) extends CompactArrayPool.Element {
    var instance: Option[Entity] = None
  }

}

final class SaveStateSystemImpl extends SaveStateSystem {

  val savedTowers = new CompactArrayPool[SavedTower]()
  val entityToTower = new mutable.HashMap[Entity, SavedTower]()

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
  }

  override def registerConnection(a: Slot, b: Slot): Unit = {
  }

  override def unregisterConnection(a: Slot, b: Slot): Unit = {
  }

  override def recreateMissingEntities(): Unit = {

    // First create towers
    for (tower <- savedTowers if tower.instance.isEmpty) {
      val entity = entitySystem.create(tower.entityType.get, tower.position, tower.rotation)
      tower.instance = Some(entity)
      entityToTower(entity) = tower
      entity.setFlag(Flag_SaveState)
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

    val Version = 1
    buffer.putMagic("s2ts")
    buffer.putInt(Version)

    buffer.putInt(linearTypes.length)
    buffer.putInt(savedTowers.length)

    for (typ <- linearTypes) {
      buffer.putIdentifier(typ.toString)
    }

    for (tower <- savedTowers) {
      val typeIndex = types(tower.entityType.name)
      buffer.putInt(typeIndex)
      buffer.putVector3(tower.position)
      buffer.putQuaternion(tower.rotation)
    }

    buffer.putMagic("E.ts")
  }

  override def load(buffer: ByteBuffer): Unit = {

    savedTowers.clear()
    entityToTower.clear()

    val MaxVersion = 1
    buffer.verifyMagic("s2ts")
    val version = buffer.getVersion(MaxVersion)

    val numTypes = buffer.getInt()
    val numTowers = buffer.getInt()

    val types = Array.fill(numTypes) {
      EntityTypeAsset(buffer.getIdentifier())
    }

    for (i <- 0 until numTowers) {
      val typeIndex = buffer.getInt()
      val typ = types(typeIndex)

      val pos = buffer.getVector3()
      val rot = buffer.getQuaternion()

      savedTowers.add(new SavedTower(typ, pos, rot))
    }

    buffer.verifyMagic("E.ts")
  }

}

