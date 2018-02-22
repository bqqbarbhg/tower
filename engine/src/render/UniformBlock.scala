package render

import java.nio.ByteBuffer
import collection.mutable.ArrayBuffer

import core._
import UniformBlock._

object UniformBlock {

  /**
    * Uniform variable passed to the shader.
    *
    * @param name Name of the uniform in the shader
    * @param offsetInVec4 Offset of the uniform (in units of vec4 ie. 16 bytes)
    * @param arraySize Number of elements in an array (0 for scalar)
    */
  sealed abstract class Uniform(val name: String, val offsetInVec4: Int, val arraySize: Int) {
    def elementSizeInVec4: Int
    def sizeInVec4: Int = math.max(arraySize, 1) * elementSizeInVec4
  }

  class UVec4(name: String, offset: Int, array: Int) extends Uniform(name, offset, array) {
    def elementSizeInVec4: Int = 1

    def set(buffer: ByteBuffer, x: Float, y: Float, z: Float, w: Float): Unit = {
      val base = offsetInVec4 * 16
      buffer.putFloat(base + 0 *4, x)
      buffer.putFloat(base + 1 *4, y)
      buffer.putFloat(base + 2 *4, z)
      buffer.putFloat(base + 3 *4, w)
    }
  }

  class UMat4(name: String, offset: Int, array: Int) extends Uniform(name, offset, array) {
    def elementSizeInVec4: Int = 4

    def set(buffer: ByteBuffer, value: Matrix4): Unit = {
      val base = offsetInVec4 * 16
      buffer.putFloat(base + 0 *4, value.m11.toFloat)
      buffer.putFloat(base + 1 *4, value.m12.toFloat)
      buffer.putFloat(base + 2 *4, value.m13.toFloat)
      buffer.putFloat(base + 3 *4, value.m14.toFloat)
      buffer.putFloat(base + 4 *4, value.m21.toFloat)
      buffer.putFloat(base + 5 *4, value.m22.toFloat)
      buffer.putFloat(base + 6 *4, value.m23.toFloat)
      buffer.putFloat(base + 7 *4, value.m24.toFloat)
      buffer.putFloat(base + 8 *4, value.m31.toFloat)
      buffer.putFloat(base + 9 *4, value.m32.toFloat)
      buffer.putFloat(base + 10*4, value.m33.toFloat)
      buffer.putFloat(base + 11*4, value.m34.toFloat)
      buffer.putFloat(base + 12*4, value.m41.toFloat)
      buffer.putFloat(base + 13*4, value.m42.toFloat)
      buffer.putFloat(base + 14*4, value.m43.toFloat)
      buffer.putFloat(base + 15*4, value.m44.toFloat)
    }
  }

  class UMat4x3(name: String, offset: Int, array: Int) extends Uniform(name, offset, array) {
    def elementSizeInVec4: Int = 3

    def set(buffer: ByteBuffer, value: Matrix43): Unit = {
      val base = offsetInVec4 * 16
      buffer.putFloat(base + 0 *4, value.m11.toFloat)
      buffer.putFloat(base + 1 *4, value.m12.toFloat)
      buffer.putFloat(base + 2 *4, value.m13.toFloat)
      buffer.putFloat(base + 3 *4, value.m14.toFloat)
      buffer.putFloat(base + 4 *4, value.m21.toFloat)
      buffer.putFloat(base + 5 *4, value.m22.toFloat)
      buffer.putFloat(base + 6 *4, value.m23.toFloat)
      buffer.putFloat(base + 7 *4, value.m24.toFloat)
      buffer.putFloat(base + 8 *4, value.m31.toFloat)
      buffer.putFloat(base + 9 *4, value.m32.toFloat)
      buffer.putFloat(base + 10*4, value.m33.toFloat)
      buffer.putFloat(base + 11*4, value.m34.toFloat)
    }
  }

  private var serialCounter: Int = 0
  def nextSerial(): Int = UniformBlock.synchronized {
    serialCounter += 1
    serialCounter - 1
  }
  def maxSerial: Int = UniformBlock.synchronized { serialCounter }
}

class UniformBlock(val name: String) {
  val serial = UniformBlock.nextSerial()

  def sizeInBytes: Int = layoutPosition * 16

  private var layoutPosition = 0
  private val uniforms = ArrayBuffer[Uniform]()

  private def push[T <: Uniform](uniform: T): T = {
    uniforms += uniform
    layoutPosition += uniform.sizeInVec4
    uniform
  }

  def vec4(name: String): UVec4 = push(new UVec4(name, layoutPosition, 0))
  def mat4(name: String): UMat4 = push(new UMat4(name, layoutPosition, 0))
  def mat4(name: String, arraySize: Int): UMat4 = push(new UMat4(name, layoutPosition, arraySize))
  def mat4x3(name: String): UMat4x3 = push(new UMat4x3(name, layoutPosition, 0))
  def mat4x3(name: String, arraySize: Int): UMat4x3 = push(new UMat4x3(name, layoutPosition, arraySize))

}

