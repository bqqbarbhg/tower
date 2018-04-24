package game.component

import asset._
import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object ItemComponent extends ComponentType("Item") {
  private val arr = MacroPropertySet.make[ItemComponent]()
  private val propertySet: PropertySet = new PropertySet("ItemComponent", arr)
  override def make = new ItemComponent
  override type Type = ItemComponent
}

class ItemComponent extends Component {
  override def propertySet: PropertySet = ItemComponent.propertySet
  override def componentType = ItemComponent

  /** Category of the item */
  var category: IntProp.Type = -1

  /** Index in the category */
  var index: IntProp.Type = -1

  /** Entity to build */
  var entity: StringProp.Type = ""

  /** Asset for the buildable entity */
  def entityAsset = EntityTypeAsset(entity)

  override def assets = Some(entityAsset)

}

