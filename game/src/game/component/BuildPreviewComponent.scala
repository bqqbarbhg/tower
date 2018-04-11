package game.component

import asset.{LoadableAsset, ModelAsset, TextureAsset}
import core._
import game.system.Entity
import io.property._
import game.system.rendering._

object BuildPreviewComponent extends ComponentType("BuildPreview") {
  private val arr = MacroPropertySet.make[BuildPreviewComponent]()
  private val propertySet: PropertySet = new PropertySet("BuildPreviewComponent", arr)
  override def make = new BuildPreviewComponent
}

class BuildPreviewComponent extends Component {
  override def propertySet: PropertySet = BuildPreviewComponent.propertySet
  override def componentType: ComponentType = BuildPreviewComponent

  var model: IdentifierProp.Type = Identifier.Empty
  var mask: IdentifierProp.Type = Identifier.Empty

  override def assets: Iterable[LoadableAsset] = Seq(ModelAsset(model), TextureAsset(mask))
}

