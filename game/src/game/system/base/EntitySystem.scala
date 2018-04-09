package game.system.base

import core.ArrayPool
import game.system._
import game.system.rendering._
import game.system.Entity._

import scala.collection.mutable.ArrayBuffer

sealed trait EntitySystem {

  /** Register entity into the system */
  def registerEntity(entity: Entity): Int

  /** Queue the removel of an entity from the world */
  def delete(entity: Entity): Unit

  /** Actually remove the entities from all the systems */
  def processDeletions(): Unit

  /** Add a listener to be informed about deleted entities. */
  def addDeleteListener(listener: EntityDeleteListener): Unit

  /** Remove a previously attached deletion listener */
  def removeDeleteListener(listener: EntityDeleteListener): Unit

  /** Remove all entities from all systems */
  def deleteAllEntities(): Unit
}

final class EntitySystemImpl extends EntitySystem {

  val allEntities = new ArrayPool[Entity]()
  val setToDelete = new EntitySet()
  val queuedDeletes = new ArrayBuffer[Entity]()
  val deleteListeners = new ArrayBuffer[EntityDeleteListener]()

  override def registerEntity(entity: Entity): Int = allEntities.add(entity)

  override def delete(entity: Entity): Unit = {
    if (entity.hasFlag(Flag_Deleted)) return
    entity.setFlag(Flag_Deleted)
    queuedDeletes += entity
  }

  override def processDeletions(): Unit = {

    // Loop in case deletes cause more entities to be deleted
    while (queuedDeletes.nonEmpty) {
      for (entity <- queuedDeletes)
        setToDelete.add(entity)
      queuedDeletes.clear()

      for (e <- setToDelete.all) {
        allEntities.remove(e.poolIndex)
      }

      for (listener <- deleteListeners) {
        listener.entitiesDeleted(setToDelete)
      }

      setToDelete.clear()
    }
  }

  override def addDeleteListener(listener: EntityDeleteListener): Unit = deleteListeners += listener
  override def removeDeleteListener(listener: EntityDeleteListener): Unit = deleteListeners -= listener

  override def deleteAllEntities(): Unit = {
    for (entity <- allEntities) {
      entity.delete()
    }

    processDeletions()
  }
}