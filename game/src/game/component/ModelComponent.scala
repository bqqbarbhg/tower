package game.component

import asset._
import core._
import game.system.Entity
import game.system.rendering._
import io.property._

object ModelComponent extends ComponentType("Model") {
  private val arr = MacroPropertySet.make[ModelComponent]()
  private val propertySet: PropertySet = new PropertySet("ModelComponent", arr)
  override def make = new ModelComponent
}

class ModelComponent extends Component {
  override def propertySet: PropertySet = ModelComponent.propertySet
  override def componentType: ComponentType = ModelComponent

  /** Model asset to use */
  var asset: IdentifierProp.Type = Identifier.Empty

  override def assets: Iterable[LoadableAsset] = Some(ModelAsset(asset))

  override def create(entity: Entity): Unit = {
    modelSystem.addModel(entity, asset)
  }
}

