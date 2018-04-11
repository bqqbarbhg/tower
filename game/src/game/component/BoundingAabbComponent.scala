package game.component

import core.Vector3
import game.system.Entity
import io.property._
import game.system.rendering._
import util.geometry.Aabb

object BoundingAabbComponent extends ComponentType("BoundingAabb") {
  private val arr = MacroPropertySet.make[BoundingAabbComponent]()
  private val propertySet: PropertySet = new PropertySet("BoundingAabbComponent", arr)
  override def make = new BoundingAabbComponent
}

class BoundingAabbComponent extends Component {
  override def propertySet: PropertySet = BoundingAabbComponent.propertySet
  override def componentType: ComponentType = BoundingAabbComponent

  /** Minimum corner */
  var min: Vector3Prop.Type = Vector3.Zero

  /** Maximum corner */
  var max: Vector3Prop.Type = Vector3.Zero

  override def create(entity: Entity): Unit = {
    cullingSystem.addAabb(entity, Aabb.fromMinMax(min, max), CullingSystem.MaskRender)
  }
}

