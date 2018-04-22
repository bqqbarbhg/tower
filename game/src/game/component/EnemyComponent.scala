package game.component

import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object EnemyComponent extends ComponentType("Enemy") {
  private val arr = MacroPropertySet.make[EnemyComponent]()
  private val propertySet: PropertySet = new PropertySet("EnemyComponent", arr)
  override def make = new EnemyComponent
  override type Type = EnemyComponent
}

class EnemyComponent extends Component {
  override def propertySet: PropertySet = EnemyComponent.propertySet
  override def componentType = EnemyComponent

  /** Local position turrets will aim for */
  var aimPosition: Vector3Prop.Type = Vector3.Zero

  /** How much health (hitpoints) does the enemy have* */
  var health: DoubleProp.Type = 100.0

  /** Area in which the enemy collides to structures and attacks */
  var hitRadius: DoubleProp.Type = 4.0

  /** How many seconds it takes the enemy to melee attack */
  var meleeDuration: DoubleProp.Type = 0.5

  /** Time into the hit to do the damage */
  var meleeHitTime: DoubleProp.Type = 0.4

  /** How much damage the enemy does to towers */
  var meleeDamage: DoubleProp.Type = 10.0

  /** How long to wait until deleting the entity after defeat */
  var defeatDuration: DoubleProp.Type = 0.0

  /** Animation to play when moving forwards */
  var moveAnim: IdentifierProp.Type = Identifier.Empty

  /** Animation to play when attacking */
  var attackAnim: IdentifierProp.Type = Identifier.Empty

  /** Animation to play when defeated */
  var defeatAnim: IdentifierProp.Type = Identifier.Empty

  /** Amount to rotate towards target exponentially */
  var rotateSpeedExponential: DoubleProp.Type = 0.1

  /** Amount to rotate towards target linearly */
  var rotateSpeedLinear: DoubleProp.Type = 0.1

  override def dependencies = Some(ModelComponent)

  override def create(entity: Entity): Unit = {
    enemySystem.addEnemy(entity, this)
  }

}

