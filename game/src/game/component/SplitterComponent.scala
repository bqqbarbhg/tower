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

  var input: SlotInfoProp.Type = new SlotInfo(true, "slot.splitter.input", Identifier("input"))
  var outputA: SlotInfoProp.Type = new SlotInfo(false, "slot.splitter.outputA", Identifier("outputA"))
  var outputB: SlotInfoProp.Type = new SlotInfo(false, "slot.splitter.outputB", Identifier("outputB"))

  override def create(entity: Entity): Unit = {
    towerSystem.addComponent(entity, this)
  }

}

