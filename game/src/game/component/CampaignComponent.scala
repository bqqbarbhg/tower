package game.component

import asset._
import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object CampaignComponent extends ComponentType("Campaign") {
  private val arr = MacroPropertySet.make[CampaignComponent]()
  private val propertySet: PropertySet = new PropertySet("CampaignComponent", arr)
  override def make = new CampaignComponent
  override type Type = CampaignComponent
}

class CampaignComponent extends Component {
  override def propertySet: PropertySet = CampaignComponent.propertySet
  override def componentType = CampaignComponent

  /** Is the campaign a tutorial */
  var enableTutorial: BoolProp.Type = false

  /** Locale key base for the campaign */
  var locale: StringProp.Type = ""

  /** Non-localized name for the campaign */
  var name: StringProp.Type = ""

  /** Root folder for the spawns */
  var spawnRoot: StringProp.Type = ""

  /** How much money to start the game with */
  var startMoney: IntProp.Type = 0

  /** Where to sort in a list of campaigns */
  var sortIndex: IntProp.Type = 0

  lazy val spawns = {
    val pack = io.content.Package.get
    val spawns = pack.list(spawnRoot).filter(_.name.endsWith(".s2es"))
    spawns.map(file => EntityTypeAsset(file.name))
  }

  override def assets = spawns

}

