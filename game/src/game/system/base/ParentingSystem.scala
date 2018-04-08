package game.system.base

import game.system._
import game.system.Entity._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait ParentingSystem extends EntityDeleteListener {

  /** Make `child` parented to `parent` */
  def parentEntity(child: Entity, parent: Entity): Unit

}


final class ParentingSystemImpl extends ParentingSystem {

  val entityToChildren = new mutable.HashMap[Entity, ArrayBuffer[Entity]]()
  val entityToParent = new mutable.HashMap[Entity, Entity]()

  override def parentEntity(child: Entity, parent: Entity): Unit = {
    require(!child.hasFlag(Flag_Parent), "May not parent to multiple entities")

    child.setFlag(Flag_Parent)
    parent.setFlag(Flag_Children)

    val children = entityToChildren.getOrElseUpdate(parent, new ArrayBuffer[Entity])
    children += child
    entityToParent(child) = parent
  }

  override def entitiesDeleted(entities: EntitySet): Unit = {
    for (entity <- entities.flag(Flag_Parent)) {
      val parent = entityToParent(entity)
      entityToChildren(parent) -= entity

      entity.clearFlag(Flag_Parent)
      entityToParent.remove(entity)
    }

    for (entity <- entities.flag(Flag_Children)) {
      for (child <- entityToChildren(entity)) {
        child.clearFlag(Flag_Parent)
        child.delete()
      }

      entity.clearFlag(Flag_Children)
      entityToChildren.remove(entity)
    }
  }

}
