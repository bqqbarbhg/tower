package game.component

import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object LightProbeComponent extends ComponentType("LightProbe") {
  private val arr = MacroPropertySet.make[LightProbeComponent]()
  private val propertySet: PropertySet = new PropertySet("LightProbeComponent", arr)
  override def make = new LightProbeComponent
  override type Type = LightProbeComponent
}

class LightProbeComponent extends Component {
  override def propertySet: PropertySet = LightProbeComponent.propertySet
  override def componentType = LightProbeComponent

  /** Local position of the light probe */
  var offset: Vector3Prop.Type = Vector3.Zero


  override def dependencies = Some(ModelComponent)

  override def create(entity: Entity): Unit = {
    val probe = ambientSystem.addProbe(entity, offset)

    for (model <- modelSystem.collectModels(entity)) {
      model.lightProbe = probe.irradianceProbe
    }
  }

}

