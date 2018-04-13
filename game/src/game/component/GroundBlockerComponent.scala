package game.component

import core._
import game.system.Entity
import io.property._
import game.system.gameplay._

object GroundBlockerComponent extends ComponentType("GroundBlocker") {
  private val arr = MacroPropertySet.make[GroundBlockerComponent]()
  private val propertySet: PropertySet = new PropertySet("GroundBlockerComponent", arr)
  override def make = new GroundBlockerComponent
  override type Type = GroundBlockerComponent
}

class GroundBlockerComponent extends Component {
  override def propertySet: PropertySet = GroundBlockerComponent.propertySet
  override def componentType: ComponentType = GroundBlockerComponent

  /** Minimum corner */
  var min: Vector2Prop.Type = Vector2.Zero

  /** Maximum corner */
  var max: Vector2Prop.Type = Vector2.Zero

  override def create(entity: Entity): Unit = {
    cableSystem.addGroundBlocker(entity, min, max)
  }
}

