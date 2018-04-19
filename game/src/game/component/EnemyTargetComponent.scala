package game.component

import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object EnemyTargetComponent extends ComponentType("EnemyTarget") {
  private val arr = MacroPropertySet.make[EnemyTargetComponent]()
  private val propertySet: PropertySet = new PropertySet("EnemyTargetComponent", arr)
  override def make = new EnemyTargetComponent
  override type Type = EnemyTargetComponent
}

class EnemyTargetComponent extends Component {
  override def propertySet: PropertySet = EnemyTargetComponent.propertySet
  override def componentType = EnemyTargetComponent

  /** How much do the enemies want to attack this */
  var value: DoubleProp.Type = 100.0

  override def create(entity: Entity): Unit = {
  }

}

