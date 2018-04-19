package game.component

import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object SlotInfo {
  private val arr = MacroPropertySet.make[SlotInfo]()
  val propertySet: PropertySet = new PropertySet("SlotInfo", arr)
}

class SlotInfo(initialIsInput: Boolean, initialLocale: String) extends PropertyContainer {
  override def propertySet: PropertySet = SlotInfo.propertySet

  /** Is the slot an input */
  var isInput: BoolProp.Type = initialIsInput

  /** Locale key base for the slot */
  var locale: StringProp.Type = initialLocale

  /** Prefix of the cable node output */
  var cableNode: StringProp.Type = ""

  /** Offset of the cable node GUI */
  var offset: Vector3Prop.Type = Vector3.Zero

}

object SlotInfoProp {
  type Type = SlotInfo
}

abstract class SlotInfoProp(name: String) extends PropertyProp[SlotInfo](name, SlotInfo.propertySet, () => ???)

