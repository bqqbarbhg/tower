package game.system

package object base {

  var entitySystem: EntitySystem = null

  def load(): Unit = {
    entitySystem = new EntitySystemImpl()
  }

}
