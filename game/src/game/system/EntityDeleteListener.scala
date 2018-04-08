package game.system

/** Trait that gets informed every frame about deleted entities */
trait EntityDeleteListener {

  /** Called with a set of entities to be removed this frame. */
  def entitiesDeleted(entities: EntitySet): Unit

}


