package game.component

import asset.SoundAsset
import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._
import SoundInfo._
import game.system.audio.AudioSystem

object SoundInfo {
  private val arr = MacroPropertySet.make[SoundInfo]()
  val propertySet: PropertySet = new PropertySet("SoundInfo", arr)

  val SfxChannel = Identifier("sfx")
  val MusicChannel = Identifier("music")
  val UiChannel = Identifier("ui")

  def getAudioChannel(name: Identifier): AudioSystem.AudioChannel.AudioChannel = {
    if (name == SfxChannel) {
      AudioSystem.Sfx
    } else if (name == UiChannel) {
      AudioSystem.Ui
    } else if (name == MusicChannel) {
      AudioSystem.Music
    } else {
      AudioSystem.Sfx
    }
  }

}

class SoundInfo extends PropertyContainer {
  override def propertySet: PropertySet = SoundInfo.propertySet

  /** Sound asset name to use */
  var sound: IdentifierProp.Type = Identifier.Empty

  /** Pitch to play with */
  var pitch: DoubleProp.Type = 1.0

  /** Volume to play with */
  var volume: DoubleProp.Type = 1.0

  /** Channel to play in */
  var channel: IdentifierProp.Type = SfxChannel

  /** How fast does the volume drop to a quarter of the original */
  var attenuation: DoubleProp.Type = 60.0

  /** Actual audio channel enum to use */
  def audioChannel: AudioSystem.AudioChannel.AudioChannel = getAudioChannel(channel)

  def asset: Option[SoundAsset] = if (sound != Identifier.Empty)
    Some(SoundAsset(sound))
  else
    None

}

object SoundInfoProp {
  type Type = SoundInfo
}

abstract class SoundInfoProp(name: String) extends PropertyProp[SoundInfo](name, SoundInfo.propertySet, () => new SoundInfo)

