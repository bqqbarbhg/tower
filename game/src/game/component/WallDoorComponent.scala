package game.component

import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object WallDoorComponent extends ComponentType("WallDoor") {
  private val arr = MacroPropertySet.make[WallDoorComponent]()
  private val propertySet: PropertySet = new PropertySet("WallDoorComponent", arr)
  override def make = new WallDoorComponent
  override type Type = WallDoorComponent
}

class WallDoorComponent extends Component {
  override def propertySet: PropertySet = WallDoorComponent.propertySet
  override def componentType = WallDoorComponent

  /** Name of the moving part bone */
  var doorBone: IdentifierProp.Type = Identifier.Empty

  /** Amount to move to closed position */
  var moveAmount: Vector3Prop.Type = Vector3.Zero

  /** How fast to open */
  var openDuration: DoubleProp.Type = 0.5

  /** Minimum corner to block when closed */
  var blockMin: Vector3Prop.Type = Vector3.Zero

  /** Maximum corner to block when closed */
  var blockMax: Vector3Prop.Type = Vector3.Zero

  /** Time closed after getting a message */
  var closeTime: DoubleProp.Type = 5.0

  var open: SlotInfoProp.Type = new SlotInfo(true, "slot.wallDoor.open")

  override def dependencies = Some(ModelComponent)

  override def create(entity: Entity): Unit = {
    towerSystem.addComponent(entity, this)
  }

}

