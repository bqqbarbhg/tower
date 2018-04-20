package game.component

import asset._
import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object DestroyableTowerComponent extends ComponentType("DestroyableTower") {
  private val arr = MacroPropertySet.make[DestroyableTowerComponent]()
  private val propertySet: PropertySet = new PropertySet("DestroyableTowerComponent", arr)
  override def make = new DestroyableTowerComponent
  override type Type = DestroyableTowerComponent
}

class DestroyableTowerComponent extends Component {
  override def propertySet: PropertySet = DestroyableTowerComponent.propertySet
  override def componentType = DestroyableTowerComponent

  /** How much health does the tower have */
  var health: DoubleProp.Type = 100.0

  override def create(entity: Entity): Unit = {
    towerSystem.addDestroyableTower(entity, this)
  }

}

