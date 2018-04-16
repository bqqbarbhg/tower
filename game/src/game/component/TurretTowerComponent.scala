package game.component

import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object TurretTowerComponent extends ComponentType("TurretTower") {
  private val arr = MacroPropertySet.make[TurretTowerComponent]()
  private val propertySet: PropertySet = new PropertySet("TurretTowerComponent", arr)
  override def make = new TurretTowerComponent
  override type Type = TurretTowerComponent
}

class TurretTowerComponent extends Component {
  override def propertySet: PropertySet = TurretTowerComponent.propertySet
  override def componentType = TurretTowerComponent

  /** Bone moved to aim direction */
  var aimBone: IdentifierProp.Type = Identifier.Empty

  /** Bone spun when firing */
  var spinBone: IdentifierProp.Type = Identifier.Empty

  /** How fast does the turret aim */
  var turnSpeed: DoubleProp.Type = 5.0

  /** How fast does the visual follow the actual aim */
  var visualTurnSpeed: DoubleProp.Type = 15.0

  /** Speed remaining per 1/60s */
  var visualTurnFriction: DoubleProp.Type = 0.9

  /** How fast does `spinBone` spin */
  var visualSpinSpeed: DoubleProp.Type = 5.0

  /** Speed remaining per 1/60s */
  var visualSpinFriction: DoubleProp.Type = 0.9

  override def dependencies: Iterable[ComponentType] = Some(ModelComponent)

  override def create(entity: Entity): Unit = {
    towerSystem.addComponent(entity, this)
  }

}
