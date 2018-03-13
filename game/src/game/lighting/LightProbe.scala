package game.lighting

import java.nio.ByteBuffer

import core._

object LightProbe {

  def make(): LightProbe = new AmbientCube()
  val SizeInVec4 = 6

  lazy val Empty = LightProbe.make()

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

}
