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

  override def create(entity: Entity): Unit = {
    enemySystem.addEnemy(entity, this)
  }

}

