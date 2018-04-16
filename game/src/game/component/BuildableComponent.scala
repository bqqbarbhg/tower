package game.component

import asset.{LoadableAsset, SoundAsset}
import core._
import game.system.Entity
import io.property._
import game.system.rendering._

object BuildableComponent extends ComponentType("Buildable") {
  private val arr = MacroPropertySet.make[BuildableComponent]()
  private val propertySet: PropertySet = new PropertySet("BuildableComponent", arr)
  override def make = new BuildableComponent
  override type Type = BuildableComponent
}

class BuildableComponent extends Component {
  override def propertySet: PropertySet = BuildableComponent.propertySet
  override def componentType: ComponentType = BuildableComponent

  var icon: IdentifierProp.Type = Identifier.Empty
  var locale: StringProp.Type = ""
  var placeSound: StringProp.Type = ""
  var gridWidth: IntProp.Type = 1
  var gridHeight: IntProp.Type = 1
  var cableOffset: Vector2Prop.Type = Vector2.Zero
  var cableMinX: IntProp.Type = -1
  var cableMinY: IntProp.Type = -1
  var cableMaxX: IntProp.Type = 1
  var cableMaxY: IntProp.Type = 1

  override def assets: Iterable[LoadableAsset] = Some(SoundAsset(placeSound))
}

