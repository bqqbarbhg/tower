package game.lighting

import core._
import io.property._

object TonemapperProps {
  private val arr = MacroPropertySet.make[Tonemapper.type]()
  var Props = new PropertySet("Tonemapper", arr) {
    enum("toeLength", Vector(0.0, 1.0, 2.0, 3.0))

    range("p0.x", 0.0, 1.0)
    range("p0.y", 0.0, 1.0)
    range("p1.x", 0.0, 1.0)
    range("p1.y", 0.0, 1.0)
  }
}

object Tonemapper extends PropertyContainer {
  override def propertySet = TonemapperProps.Props

  /** Apply the tonemapping operator only to the luminance.
    * Requires shader recompilation to take effect. */
  var luminanceOnly: BoolProp.Type = false

  /** How long should the initial low-exposure exponential part of the curve be */
  var toeLength: DoubleProp.Type = 0.0

  /** First control point */
  var p0: Vector2Prop.Type = Vector2.Zero

  /** Second control point */
  var p1: Vector2Prop.Type = Vector2.Zero
}

