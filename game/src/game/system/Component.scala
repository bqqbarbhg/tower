package game.system

abstract class Component(val entity: Entity) {

  def start(): Unit = { }
  def stop(): Unit = { }

}
