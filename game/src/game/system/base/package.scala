package game.system

package object base {

  var entitySystem: EntitySystem = null
  var parentingSystem: ParentingSystem = null

  def load(): Unit = {
    entitySystem = new EntitySystemImpl()
    parentingSystem = new ParentingSystemImpl()
  }

}
