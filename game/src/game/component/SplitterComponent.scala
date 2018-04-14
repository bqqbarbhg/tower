package game.component

import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object SplitterComponent extends ComponentType("Splitter") {
  private val arr = MacroPropertySet.make[SplitterComponent]()
  private val propertySet: PropertySet = new PropertySet("SplitterComponent", arr)
  override def make = new SplitterComponent
  override type Type = SplitterComponent
}

class SplitterComponent extends Component {
  override def propertySet: PropertySet = SplitterComponent.propertySet
  override def componentType = SplitterComponent

  /** Prefix of input cable */
  var cableNodeIn: StringProp.Type = ""

  /** Prefix of first output cable */
  var cableNodeOutA: StringProp.Type = ""

  /** Prefix of second output cable */
  var cableNodeOutB: StringProp.Type = ""

  override def create(entity: Entity): Unit = {
    towerSystem.addComponent(entity, this)
  }

}

