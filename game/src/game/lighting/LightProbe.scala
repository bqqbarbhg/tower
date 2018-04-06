package game.lighting

import java.nio.ByteBuffer

import core._

object LightProbe {
  def make(): LightProbe = new SphericalHarmonic2()
  val SizeInVec4 = 9
  val Empty = new SphericalHarmonic2()
}

trait LightProbe {

  /**
    * Clear the probe to the initial (completely dark) state
    */
  def clear(): Unit

  /**
    * Copy the state from another light probe.
    *
    * @param other Another light probe _of the same type_
    */
  def copyFrom(other: LightProbe): Unit

  /**
    * Accumulate omnidirectional light.
    *
    * @param intensity Intensity of the light (RGB)
    */
  def addGlobal(intensity: Vector3): Unit

  /**
    * Accumulate directed light to the probe.
    *
    * @param direction Normalized direction from the probe to the light
    * @param intensity Intensity of the light (RGB)
    */
  def addDirectional(direction: Vector3, intensity: Vector3): Unit

  /**
    * Write the probe data to an uniform readable by a shader.
    *
    * @param b Buffer to write the probe to
    * @param offset Offset in bytes to write to
    * @param stride Distance between array elements in uniform representation.
    */
  def writeToUniform(b: ByteBuffer, offset: Int, stride: Int): Unit

  /**
    * Evaluate the light to a direction.
    *
    * @param direction Direction _to_ the coming irradiance
    * @return Light intensity to `direction`
    */
  def evaluate(direction: Vector3): Vector3

  /**
    * Multiply the light by some constant factor in-place.
    *
    * @param amount Amount to multiply the light with
    */
  def *=(amount: Double): Unit

  /**
    * Add contents of another light probe to this in-place.
    *
    * @param other Other light probe _of the same type_
    */
  def +=(other: LightProbe): Unit


  // Functions defined using the above, but may be overloaded for speed:

  /**
    * Create a copy of this light probe.
    */
  def copy: LightProbe = {
    val l = LightProbe.make()
    l.copyFrom(this)
    l
  }

  /**
    * Multiply the light by some constant factor.
    *
    * @param amount Amount to multiply the light with
    * @return The resulting multiplied probe
    */
  def *(amount: Double): LightProbe = {
    val l = this.copy
    l *= amount
    l
  }

  /**
    * Add contents of another light probe to this.
    *
    * @param other Other light probe _of the same type_
    * @return The resulting sum probe
    */
  def +(other: LightProbe): LightProbe = {
    val l = this.copy
    l += other
    l
  }

  /**
    * Add contents of another light probe scaled by some factor to this in-place.
    *
    * Equivalent to: this += other * amount
    *
    * @param other Other light probe _of the same type_
    * @param amount Amount to multiply the other light with
    */
  def addScaled(other: LightProbe, amount: Double): Unit = {
    this += other * amount
  }

  /**
    * Accumulate directed light to the probe with a multiplier.
    *
    * @param direction Normalized direction from the probe to the light
    * @param intensity Intensity of the light (RGB)
    * @param scale Multiplier for `intensity`
    */
  def addDirectionalScaled(direction: Vector3, intensity: Vector3, scale: Double): Unit = {
    addDirectional(direction, intensity * scale)
  }

  /**
    * Linearly interpolate between `a` and `b` storing the result to `this`.
    *
    * @param a First endpoint _of the same type_ (t = 0.0)
    * @param b Second endpoint _of the same type_ (t = 1.0)
    * @param t Fraction to interpolate between
    */
  def lerpFrom(a: LightProbe, b: LightProbe, t: Double): Unit = {
    this.copyFrom(a)
    this *= (1.0 - t)
    this.addScaled(b, t)
  }

  /**
    * Bilinearly interpolate between `p00,p01; p10,p11` storing the result to `this`.
    *
    * @param p00 Endpoint (x = 0, y = 0)
    * @param p01 Endpoint (x = 1, y = 0)
    * @param p10 Endpoint (x = 0, y = 1)
    * @param p11 Endpoint (x = 1, y = 1)
    * @param x X-axis of the interpolation
    * @param y Y-axis of the interpolation
    */
  def bilerpFrom(p00: LightProbe, p01: LightProbe, p10: LightProbe, p11: LightProbe, x: Double, y: Double): Unit = {
    val t0 = LightProbe.make()
    val t1 = LightProbe.make()
    t0.lerpFrom(p00, p01, x)
    t1.lerpFrom(p10, p11, x)
    this.lerpFrom(t0, t1, y)
  }

}
