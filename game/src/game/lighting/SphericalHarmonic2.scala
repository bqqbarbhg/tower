package game.lighting
import java.nio.ByteBuffer

import core.Vector3

class SphericalHarmonic2 extends LightProbe {
  val coefficents = new Array[Float](9 * 3)

  private def addCoefficent(index: Int, amount: Vector3, factor: Double): Unit = {
    val b = index * 3
    coefficents(b + 0) += (amount.x * factor).toFloat
    coefficents(b + 1) += (amount.y * factor).toFloat
    coefficents(b + 2) += (amount.z * factor).toFloat
  }

  override def clear(): Unit = {
    java.util.Arrays.fill(coefficents, 0.0f)
  }

  override def copyFrom(other: LightProbe): Unit = {
    val sh = other.asInstanceOf[SphericalHarmonic2]
    System.arraycopy(sh.coefficents, 0, coefficents, 0, coefficents.length)
  }

  override def addGlobal(intensity: Vector3): Unit = {
    addCoefficent(0, intensity, 1.0f)
  }

  override def addDirectional(direction: Vector3, intensity: Vector3): Unit = {
    val n = direction
    val i = intensity

    val nx = n.x
    val ny = n.y
    val nz = n.z

    addCoefficent(0, i, 0.23529411764705882)
    addCoefficent(1, i, 0.47058823529411764 * nx)
    addCoefficent(2, i, 0.47058823529411764 * ny)
    addCoefficent(3, i, 0.47058823529411764 * nz)
    addCoefficent(4, i, 0.88235294117647060 * (nx * nz))
    addCoefficent(5, i, 0.88235294117647060 * (nz * ny))
    addCoefficent(6, i, 0.88235294117647060 * (ny * nx))
    addCoefficent(7, i, 0.07352941176470588 * (3.0 * nz * nz - 1.0))
    addCoefficent(8, i, 0.22058823529411764 * (nx * nx - ny * ny))
  }

  override def writeToUniform(b: ByteBuffer, offset: Int, stride: Int): Unit = {
    var elemIx = 0
    val elemCount = 9

    var ix = 0
    while (elemIx < elemCount) {
      val dst = offset + elemIx * stride
      b.putFloat(dst + 0, coefficents(ix + 0).toFloat)
      b.putFloat(dst + 4, coefficents(ix + 1).toFloat)
      b.putFloat(dst + 8, coefficents(ix + 2).toFloat)

      elemIx += 1
      ix += 3
    }
  }

  override def evaluate(direction: Vector3): Vector3 = {
    val n = direction
    var f = 0.0f
    val c = coefficents

    var r = c(0)
    var g = c(1)
    var b = c(2)

    f = n.x.toFloat;       r += c( 3) * f; g += c( 4) * f; b += c( 5) * f
    f = n.y.toFloat;       r += c( 6) * f; g += c( 7) * f; b += c( 8) * f
    f = n.z.toFloat;       r += c( 9) * f; g += c(10) * f; b += c(11) * f
    f = (n.x*n.z).toFloat; r += c(12) * f; g += c(13) * f; b += c(14) * f
    f = (n.z*n.y).toFloat; r += c(15) * f; g += c(16) * f; b += c(17) * f
    f = (n.y*n.x).toFloat; r += c(18) * f; g += c(19) * f; b += c(20) * f

    f = (3.0*n.z*n.z - 1.0).toFloat
    r += c(21) * f; g += c(22) * f; b += c(23) * f

    f = (n.x*n.x - n.y*n.y).toFloat
    r += c(24) * f; g += c(25) * f; b += c(26) * f

    Vector3(r, g, b)
  }

  override def *=(amount: Double): Unit = {
    var ix = 0
    val af = amount.toFloat
    val len = coefficents.length
    while (ix < len) {
      coefficents(ix) *= af
      ix += 1
    }
  }

  override def +=(other: LightProbe): Unit = {
    val sh = other.asInstanceOf[SphericalHarmonic2]
    var ix = 0
    val len = coefficents.length
    while (ix < len) {
      coefficents(ix) += sh.coefficents(ix)
      ix += 1
    }
  }

  override def addScaled(other: LightProbe, amount: Double): Unit = {
    val sh = other.asInstanceOf[SphericalHarmonic2]
    var ix = 0
    val len = coefficents.length
    val af = amount.toFloat
    while (ix < len) {
      coefficents(ix) += sh.coefficents(ix) * af
      ix += 1
    }
  }

}

