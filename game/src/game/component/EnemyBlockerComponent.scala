package game.component

import core._
import game.system.Entity
import io.property._
import game.system.gameplay._

object EnemyBlockerComponent extends ComponentType("EnemyBlocker") {
  private val arr = MacroPropertySet.make[EnemyBlockerComponent]()
  private val propertySet: PropertySet = new PropertySet("EnemyBlockerComponent", arr)
  override def make = new EnemyBlockerComponent
  override type Type = EnemyBlockerComponent
}

class EnemyBlockerComponent extends Component {
  override def propertySet: PropertySet = EnemyBlockerComponent.propertySet
  override def componentType: ComponentType = EnemyBlockerComponent

  /** Minimum corner */
  var min: Vector2Prop.Type = Vector2.Zero

  /** Maximum corner */
  var max: Vector2Prop.Type = Vector2.Zero

  override def create(entity: Entity): Unit = {
  }
}

