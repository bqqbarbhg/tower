package game.component

import asset._
import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object BreakableComponent extends ComponentType("Breakable") {
  private val arr = MacroPropertySet.make[BreakableComponent]()
  private val propertySet: PropertySet = new PropertySet("BreakableComponent", arr)
  override def make = new BreakableComponent
  override type Type = BreakableComponent
}

class BreakableComponent extends Component {
  override def propertySet: PropertySet = BreakableComponent.propertySet
  override def componentType = BreakableComponent

  /** Entity to use for the effect on unintended breakage */
  var hardBreakEffect: StringProp.Type = ""

  /** Entity to use for the effect on manual deletion */
  var softBreakEffect: StringProp.Type = ""

  override def assets = Array(EntityTypeAsset(hardBreakEffect), EntityTypeAsset(softBreakEffect))

  override def create(entity: Entity): Unit = {
  }

}

