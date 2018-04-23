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
  var turnSpeed: DoubleProp.Type = 7.0

  /** How fast does the visual follow the actual aim */
  var visualTurnSpeed: DoubleProp.Type = 35.0

  /** Speed remaining per 1/60s */
  var visualTurnFriction: DoubleProp.Type = 0.9

  /** How fast does `spinBone` spin */
  var visualSpinSpeed: DoubleProp.Type = 5.0

  /** Speed remaining per 1/60s */
  var visualSpinFriction: DoubleProp.Type = 0.9

  /** Time interval between shots */
  var shootInterval: DoubleProp.Type = 0.3

  /** Where to do the raycast to target */
  var shootOrigin: Vector3Prop.Type = Vector3.Zero

  /** How far to shoot */
  var shootDistance: DoubleProp.Type = 10.0

  /** How long does it take to start shooting after locking onto target */
  var aimDuration: DoubleProp.Type = 0.7

  /** How much damage does the turret do per shot */
  var shootDamage: DoubleProp.Type = 10.0

  /** Time in seconds to shoot an outdated target */
  var shootTime: DoubleProp.Type = 3.0

  /** Distance from `shootOrigin` to spawn bullets from */
  var bulletExitDistance: DoubleProp.Type = 0.0

  /** Distance from `shootOrigin` to spawn smoke from */
  var smokeExitDistance: DoubleProp.Type = 0.0

  /** Random offset the shot bullets */
  var spread: Vector3Prop.Type = Vector3.Zero

  /** Exponential interpolation towards visual yaw */
  var visualYawExponential: DoubleProp.Type = 0.15

  /** Linear interpolation towards visual yaw */
  var visualYawLinear: DoubleProp.Type = 0.1

  /** Slot */
  var targetIn: SlotInfoProp.Type = new SlotInfo(true, "slot.turret.targetIn", Identifier("targetIn"))

  override def dependencies: Iterable[ComponentType] = Some(ModelComponent)

  override def create(entity: Entity): Unit = {
    towerSystem.addComponent(entity, this)
  }

}
