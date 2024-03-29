package game.lighting
import java.nio.ByteBuffer

import core._

/**
  * Cube-shaped light probe. Contains light intensity from six axis-aligned sides.
  *
  * See: http://www.valvesoftware.com/publications/2006/SIGGRAPH06_Course_ShadingInValvesSourceEngine.pdf
  */
class AmbientCube extends LightProbe {
  var coefficents = Array.fill(3 * 6)(0.0)

  override def clear(): Unit = {
    java.util.Arrays.fill(coefficents, 0.0)
  }

  override def copyFrom(other: LightProbe): Unit = {
    val cube = other.asInstanceOf[AmbientCube]
    java.lang.System.arraycopy(cube.coefficents, 0, coefficents, 0, coefficents.length)
  }

  override def addGlobal(intensity: Vector3): Unit = {
    var side = 0
    while (side < 6) {
      val base = side * 3
      coefficents(base + 0) += intensity.x
      coefficents(base + 1) += intensity.y
      coefficents(base + 2) += intensity.z
      side += 1
    }
  }

  override def addDirectional(direction: Vector3, intensity: Vector3): Unit = {
    val fx = direction.x * direction.x
    val fy = direction.y * direction.y
    val fz = direction.z * direction.z

    if (direction.x > 0) {
      coefficents( 0) += intensity.x * fx
      coefficents( 1) += intensity.y * fx
      coefficents( 2) += intensity.z * fx
    } else {
      coefficents( 3) += intensity.x * fx
      coefficents( 4) += intensity.y * fx
      coefficents( 5) += intensity.z * fx
    }

    if (direction.y > 0) {
      coefficents( 6) += intensity.x * fy
      coefficents( 7) += intensity.y * fy
      coefficents( 8) += intensity.z * fy
    } else {
      coefficents( 9) += intensity.x * fy
      coefficents(10) += intensity.y * fy
      coefficents(11) += intensity.z * fy
    }

    if (direction.z > 0) {
      coefficents(12) += intensity.x * fz
      coefficents(13) += intensity.y * fz
      coefficents(14) += intensity.z * fz
    } else {
      coefficents(15) += intensity.x * fz
      coefficents(16) += intensity.y * fz
      coefficents(17) += intensity.z * fz
    }
  }

  override def writeToUniform(b: ByteBuffer, offset: Int, stride: Int): Unit = {
    var side = 0
    while (side < 6) {
      val src = side * 3
      val dst = offset + side * stride
      b.putFloat(dst + 0, coefficents(src + 0).toFloat)
      b.putFloat(dst + 4, coefficents(src + 1).toFloat)
      b.putFloat(dst + 8, coefficents(src + 2).toFloat)
      side += 1
    }
  }

  override def evaluate(direction: Vector3): Vector3 = {
    val fx = direction.x * direction.x
    val fy = direction.y * direction.y
    val fz = direction.z * direction.z
    val ox = if (direction.x >= 0.0) 0  else 3
    val oy = if (direction.x >= 0.0) 6  else 9
    val oz = if (direction.x >= 0.0) 12 else 15

    val c = coefficents
    val r = c(ox + 0) * fx + c(oy + 0) * fy + c(oz + 0) * fz
    val g = c(ox + 1) * fx + c(oy + 1) * fy + c(oz + 1) * fz
    val b = c(ox + 2) * fx + c(oy + 2) * fy + c(oz + 2) * fz

    Vector3(r, g, b)
  }

  override def *=(amount: Double): Unit = {
    var ix = 0
    val len = coefficents.length
    while (ix < len) {
      coefficents(ix) *= amount
      ix += 1
    }
  }

  override def +=(other: LightProbe): Unit = {
    val cube = other.asInstanceOf[AmbientCube]
    var ix = 0
    val len = coefficents.length
    while (ix < len) {
      coefficents(ix) += cube.coefficents(ix)
      ix += 1
    }
  }
}

