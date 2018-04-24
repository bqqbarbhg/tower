package game.component

import asset.SoundAsset
import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object SoundInfo {
  private val arr = MacroPropertySet.make[SoundInfo]()
  val propertySet: PropertySet = new PropertySet("SoundInfo", arr)
}

class SoundInfo extends PropertyContainer {
  override def propertySet: PropertySet = SoundInfo.propertySet

  /** Sound asset name to use */
  var sound: IdentifierProp.Type = Identifier.Empty

  /** Pitch to play with */
  var pitch: DoubleProp.Type = 1.0

  /** Volume to play with */
  var volume: DoubleProp.Type = 1.0

  def asset: Option[SoundAsset] = if (sound != Identifier.Empty)
    Some(SoundAsset(sound))
  else
    None

}

object SoundInfoProp {
  type Type = SoundInfo
}

abstract class SoundInfoProp(name: String) extends PropertyProp[SoundInfo](name, SoundInfo.propertySet, () => new SoundInfo)

