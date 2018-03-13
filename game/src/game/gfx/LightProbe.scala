package game.gfx

import java.nio.ByteBuffer

import core._

trait LightProbe {

  /**
    * Accumulate directed light to the probe.
    *
    * @param direction Normalized direction from the probe to the light
    * @param intensity Intensity of the light (RGB)
    */
  def add(direction: Vector3, intensity: Vector3): Unit

  /**
    * Write the probe data to an uniform readable by a shader.
    *
    * @param b Buffer to write the probe to
    * @param offset Offset in bytes to write to
    * @param stride Distance between array elements in uniform representation.
    */
  def writeToUniform(b: ByteBuffer, offset: Int, stride: Int): Unit

}
