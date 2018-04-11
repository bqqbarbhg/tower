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
}

class TurretTowerComponent extends Component {
  override def propertySet: PropertySet = TurretTowerComponent.propertySet
  override def componentType = TurretTowerComponent

  /** Bone moved to aim direction */
  var aimBone: IdentifierProp.Type = Identifier.Empty

  /** Bone spun when firing */
  var spinBone: IdentifierProp.Type = Identifier.Empty

  override def dependencies: Iterable[ComponentType] = Some(ModelComponent)

  override def create(entity: Entity): Unit = {
    towerSystem.addTurret(entity, this)
  }

}
