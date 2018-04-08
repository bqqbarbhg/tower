package game.system.base

import game.system._
import game.system.rendering._

import scala.collection.mutable.ArrayBuffer

sealed trait EntitySystem {

  /** Queue the removel of an entity from the world */
  def delete(entity: Entity): Unit

  /** Actually remove the entities from all the systems */
  def processDeletions(): Unit

  /** Add a listener to be informed about deleted entities. */
  def addDeleteListener(listener: EntityDeleteListener): Unit

  /** Remove a previously attached deletion listener */
  def removeDeleteListener(listener: EntityDeleteListener): Unit
}

final class EntitySystemImpl extends EntitySystem {

  val queuedDeletes = new EntitySet()
  val deleteListeners = new ArrayBuffer[EntityDeleteListener]()

  override def delete(entity: Entity): Unit = {
    queuedDeletes.add(entity)
  }

  override def processDeletions(): Unit = {
    if (queuedDeletes.nonEmpty) {

      for (listener <- deleteListeners) {
        listener.entitiesDeleted(queuedDeletes)
      }

      queuedDeletes.clear()
    }
  }

  override def addDeleteListener(listener: EntityDeleteListener): Unit = deleteListeners += listener
  override def removeDeleteListener(listener: EntityDeleteListener): Unit = deleteListeners -= listener
}