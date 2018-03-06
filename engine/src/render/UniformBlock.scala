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
    * @param offsetInBytes Offset of the uniform in bytes
    * @param arraySize Number of elements in an array (0 for scalar)
    */
  sealed abstract class Uniform(val name: String, var offsetInBytes: Int, val arraySize: Int) {

    protected var arrayStrideInBytes = elementSizeInBytes
    protected var matrixStrideInBytes = 16

    /** Is the uniform some kind of a matrix */
    def isMatrix: Boolean

    /**
      * Update the layout of this uniform with queried values. Not necessary
      * when using std140, as the precomputed layout matches it.
      *
      * @param offset Offset from the beginning of the block in bytes
      * @param arrayStride Distance between elements in an array in bytes
      * @param matrixStride Distance between rows/columns of a matrix in bytes
      */
    def updateLayout(offset: Int, arrayStride: Int, matrixStride: Int): Unit = {
      offsetInBytes = offset
      arrayStrideInBytes = arrayStride
      matrixStrideInBytes = matrixStride
    }

    def elementSizeInBytes: Int
    def sizeInBytes: Int = math.max(arraySize, 1) * elementSizeInBytes
  }

  class UVec4(name: String, offset: Int, array: Int) extends Uniform(name, offset, array) {
    override def isMatrix: Boolean = false
    override def elementSizeInBytes: Int = 16

    def setSrgb(buffer: ByteBuffer, color: Color): Unit = setSrgb(buffer, 0, color)
    def setSrgb(buffer: ByteBuffer, index: Int, color: Color): Unit = {
      val r = Color.linearToSrgb(color.r).toFloat
      val g = Color.linearToSrgb(color.g).toFloat
      val b = Color.linearToSrgb(color.b).toFloat
      set(buffer, index, r, g, b, color.a.toFloat)
    }

    def set(buffer: ByteBuffer, x: Float, y: Float, z: Float, w: Float): Unit = set(buffer, 0, x, y, z, w)
    def set(buffer: ByteBuffer, index: Int, x: Float, y: Float, z: Float, w: Float): Unit = {
      val base = offsetInBytes + index * arrayStrideInBytes
      buffer.putFloat(base + 0*4, x)
      buffer.putFloat(base + 1*4, y)
      buffer.putFloat(base + 2*4, z)
      buffer.putFloat(base + 3*4, w)
    }
  }

  class UMat4(name: String, offset: Int, array: Int) extends Uniform(name, offset, array) {
    override def isMatrix: Boolean = true
    override def elementSizeInBytes: Int = 64

    def set(buffer: ByteBuffer, value: Matrix4): Unit = set(buffer, 0, value)
    def set(buffer: ByteBuffer, index: Int, value: Matrix4): Unit = {
      val base = offsetInBytes + index * arrayStrideInBytes
      var b = base
      buffer.putFloat(b + 0*4, value.m11.toFloat)
      buffer.putFloat(b + 1*4, value.m12.toFloat)
      buffer.putFloat(b + 2*4, value.m13.toFloat)
      buffer.putFloat(b + 3*4, value.m14.toFloat)
      b += matrixStrideInBytes
      buffer.putFloat(b + 0*4, value.m21.toFloat)
      buffer.putFloat(b + 1*4, value.m22.toFloat)
      buffer.putFloat(b + 2*4, value.m23.toFloat)
      buffer.putFloat(b + 3*4, value.m24.toFloat)
      b += matrixStrideInBytes
      buffer.putFloat(b + 0*4, value.m31.toFloat)
      buffer.putFloat(b + 1*4, value.m32.toFloat)
      buffer.putFloat(b + 2*4, value.m33.toFloat)
      buffer.putFloat(b + 3*4, value.m34.toFloat)
      b += matrixStrideInBytes
      buffer.putFloat(b + 0*4, value.m41.toFloat)
      buffer.putFloat(b + 1*4, value.m42.toFloat)
      buffer.putFloat(b + 2*4, value.m43.toFloat)
      buffer.putFloat(b + 3*4, value.m44.toFloat)
    }
  }

  class UMat4x3(name: String, offset: Int, array: Int) extends Uniform(name, offset, array) {
    override def isMatrix: Boolean = true
    override def elementSizeInBytes: Int = 48

    def set(buffer: ByteBuffer, value: Matrix43): Unit = set(buffer, 0, value)
    def set(buffer: ByteBuffer, index: Int, value: Matrix43): Unit = {
      val base = offsetInBytes + index * arrayStrideInBytes
      var b = base
      buffer.putFloat(b + 0*4, value.m11.toFloat)
      buffer.putFloat(b + 1*4, value.m12.toFloat)
      buffer.putFloat(b + 2*4, value.m13.toFloat)
      buffer.putFloat(b + 3*4, value.m14.toFloat)
      b += matrixStrideInBytes
      buffer.putFloat(b + 0*4, value.m21.toFloat)
      buffer.putFloat(b + 1*4, value.m22.toFloat)
      buffer.putFloat(b + 2*4, value.m23.toFloat)
      buffer.putFloat(b + 3*4, value.m24.toFloat)
      b += matrixStrideInBytes
      buffer.putFloat(b + 0*4, value.m31.toFloat)
      buffer.putFloat(b + 1*4, value.m32.toFloat)
      buffer.putFloat(b + 2*4, value.m33.toFloat)
      buffer.putFloat(b + 3*4, value.m34.toFloat)
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

  def sizeInBytes: Int = layoutPosition
  val uniforms = ArrayBuffer[Uniform]()

  private var layoutPosition = 0

  private def push[T <: Uniform](uniform: T): T = {
    uniforms += uniform
    layoutPosition += uniform.sizeInBytes
    uniform
  }

  def vec4(name: String): UVec4 = push(new UVec4(name, layoutPosition, 0))
  def vec4(name: String, arraySize: Int): UVec4 = push(new UVec4(name, layoutPosition, arraySize))
  def mat4(name: String): UMat4 = push(new UMat4(name, layoutPosition, 0))
  def mat4(name: String, arraySize: Int): UMat4 = push(new UMat4(name, layoutPosition, arraySize))
  def mat4x3(name: String): UMat4x3 = push(new UMat4x3(name, layoutPosition, 0))
  def mat4x3(name: String, arraySize: Int): UMat4x3 = push(new UMat4x3(name, layoutPosition, arraySize))

}

