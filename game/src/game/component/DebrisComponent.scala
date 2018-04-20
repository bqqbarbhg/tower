package game.component

import asset._
import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object DebrisComponent extends ComponentType("Debris") {
  private val arr = MacroPropertySet.make[DebrisComponent]()
  private val propertySet: PropertySet = new PropertySet("DebrisComponent", arr)
  override def make = new DebrisComponent
  override type Type = DebrisComponent
}

class DebrisComponent extends Component {
  override def propertySet: PropertySet = DebrisComponent.propertySet
  override def componentType = DebrisComponent

  /** Model to use for the effect */
  var model: IdentifierProp.Type = Identifier.Empty

  /** Prefix of part nodes */
  var partPrefix: StringProp.Type = ""

  /** Center to aim parts out of */
  var center: Vector3Prop.Type = Vector3.Zero

  /** Global velocity applied to every part */
  var globalVelocity: Vector3Prop.Type = Vector3.Zero

  /** Velocity outwards from center */
  var baseVelocity: Vector3Prop.Type = Vector3(10.0, 10.0, 10.0)

  /** Random velocity to add to the pieces */
  var randomVelocity: Vector3Prop.Type = Vector3(10.0, 10.0, 10.0)

  /** Amount of random rotation to add to the parts */
  var rotationAmount: DoubleProp.Type = 1.0

  /** Speed to accelerate the debris with */
  var gravity: Vector3Prop.Type = Vector3(0.0, -20.0, 0.0)

  /** Duration in seconds the effect lasts */
  var lifetime: DoubleProp.Type = 10.0

  /** Duration in seconds during which the parts are scaled down in the end  */
  var fadeTime: DoubleProp.Type = 2.0

  /** Radius for the culling area */
  var cullRadius: DoubleProp.Type = 10.0

  override def assets = Some(ModelAsset(model))

  override def create(entity: Entity): Unit = {
    debrisSystem.addDebrisBurst(entity, this)
  }

}

