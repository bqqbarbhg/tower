package game.component

import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object DrillTowerComponent extends ComponentType("DrillTower") {
  private val arr = MacroPropertySet.make[DrillTowerComponent]()
  private val propertySet: PropertySet = new PropertySet("DrillTowerComponent", arr)
  override def make = new DrillTowerComponent
  override type Type = DrillTowerComponent
}

class DrillTowerComponent extends Component {
  override def propertySet: PropertySet = DrillTowerComponent.propertySet
  override def componentType = DrillTowerComponent

  override def dependencies = Some(ModelComponent)

  /** Bone to rotate for animation */
  var rotateBone: IdentifierProp.Type = Identifier.Empty

  /** Time in seconds to complete turns */
  var turnDuration: DoubleProp.Type = 4.0

  /** Time to wait between turns */
  var turnWaitTime: DoubleProp.Type = 1.0

  /** Chance to drill between turns */
  var drillChance: DoubleProp.Type = 0.3

  override def create(entity: Entity): Unit = {
    towerSystem.addComponent(entity, this)
  }

}

