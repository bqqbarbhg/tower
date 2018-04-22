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
  override type Type = ModelComponent
}

class ModelComponent extends Component {
  override def propertySet: PropertySet = ModelComponent.propertySet
  override def componentType: ComponentType = ModelComponent

  /** Model asset to use */
  var asset: IdentifierProp.Type = Identifier.Empty

  /** Relative scale of the model */
  var scale: Vector3Prop.Type = Vector3.One

  override def assets: Iterable[LoadableAsset] = Some(ModelAsset(asset))

  override def create(entity: Entity): Unit = {
    val model = modelSystem.addModel(entity, asset)
    model.scale = scale
  }
}

