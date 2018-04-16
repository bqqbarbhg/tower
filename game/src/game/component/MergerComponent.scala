package game.component

import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object MergerComponent extends ComponentType("Merger") {
  private val arr = MacroPropertySet.make[MergerComponent]()
  private val propertySet: PropertySet = new PropertySet("MergerComponent", arr)
  override def make = new MergerComponent
  override type Type = MergerComponent
}

class MergerComponent extends Component {
  override def propertySet: PropertySet = MergerComponent.propertySet
  override def componentType = MergerComponent

  /** Prefix of output cable */
  var cableNodeOut: StringProp.Type = ""

  /** Prefix of first input cable */
  var cableNodeInA: StringProp.Type = ""

  /** Prefix of second input cable */
  var cableNodeInB: StringProp.Type = ""

  override def create(entity: Entity): Unit = {
    towerSystem.addComponent(entity, this)
  }

}

