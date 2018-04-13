package game.system

package object base {

  var entitySystem: EntitySystem = null
  var parentingSystem: ParentingSystem = null

  def loadState(): Unit = {
    entitySystem = new EntitySystemImpl()
    parentingSystem = new ParentingSystemImpl()

    entitySystem.addDeleteListener(parentingSystem)
  }

  def unloadState(): Unit = {
  }

}
