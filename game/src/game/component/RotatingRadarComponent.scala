package game.component

import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object RotatingRadarComponent extends ComponentType("RotatingRadar") {
  private val arr = MacroPropertySet.make[RotatingRadarComponent]()
  private val propertySet: PropertySet = new PropertySet("RotatingRadarComponent", arr)
  override def make = new RotatingRadarComponent
  override type Type = RotatingRadarComponent
}

class RotatingRadarComponent extends Component {
  override def propertySet: PropertySet = RotatingRadarComponent.propertySet
  override def componentType = RotatingRadarComponent

  /** Bone moved when rotating */
  var rotateBone: IdentifierProp.Type = Identifier.Empty

  /** Detection radius for the radar */
  var radius: DoubleProp.Type = 0.0

  /** Offset for slot targetOut */
  var targetOutOffset: Vector3Prop.Type = Vector3.Zero

  var targetOut: SlotInfoProp.Type = new SlotInfo(false, "slot.radar.targetOut")

  override def dependencies: Iterable[ComponentType] = Some(ModelComponent)

  override def create(entity: Entity): Unit = {
    towerSystem.addComponent(entity, this)
  }

}

