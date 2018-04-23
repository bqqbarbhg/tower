package game.component

import asset.{LoadableAsset, SoundAsset}
import core._
import game.system.Entity
import io.property._
import game.system.gameplay._

object GridBlockComponent extends ComponentType("GridBlock") {
  private val arr = MacroPropertySet.make[GridBlockComponent]()
  private val propertySet: PropertySet = new PropertySet("GridBlockComponent", arr)
  override def make = new GridBlockComponent
  override type Type = GridBlockComponent
}

class GridBlockComponent extends Component {
  override def propertySet: PropertySet = GridBlockComponent.propertySet
  override def componentType: ComponentType = GridBlockComponent

  var width: IntProp.Type = 1
  var height: IntProp.Type = 1

  override def create(entity: Entity): Unit = {
    buildSystem.addGridBlocker(entity, this)
  }
}

