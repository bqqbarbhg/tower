package game.component

import asset.EntityTypeAsset
import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object RoundComponent extends ComponentType("Round") {
  private val arr = MacroPropertySet.make[RoundComponent]()
  private val propertySet: PropertySet = new PropertySet("RoundComponent", arr)
  override def make = new RoundComponent
  override type Type = RoundComponent
}

class RoundComponent extends Component {
  override def propertySet: PropertySet = RoundComponent.propertySet
  override def componentType = RoundComponent

  /** How much to give money to the player from completing this round */
  var rewardMoney: IntProp.Type = 0

}

