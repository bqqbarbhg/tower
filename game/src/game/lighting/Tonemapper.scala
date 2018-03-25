package game.lighting

import io.property._

object TonemapperProps {
  private val arr = MacroPropertySet.make[Tonemapper.type]()
  var Props = new PropertySet(arr) {
    enum("toeLength", Vector(0.0, 1.0, 2.0, 3.0))
  }
}

object Tonemapper extends PropertyContainer {
  override def propertySet = TonemapperProps.Props

  /** Apply the tonemapping operator only to the luminance.
    * Requires shader recompilation to take effect. */
  var luminanceOnly: BoolProp.Type = false

  /** How long should the initial low-exposure exponential part of the curve be */
  var toeLength: DoubleProp.Type = 0.0
}

