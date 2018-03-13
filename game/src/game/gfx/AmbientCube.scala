package game.gfx
import java.nio.ByteBuffer

import core._

/**
  * Cube-shaped light probe. Contains light intensity from six axis-aligned sides.
  *
  * See: http://www.valvesoftware.com/publications/2006/SIGGRAPH06_Course_ShadingInValvesSourceEngine.pdf
  */
class AmbientCube extends LightProbe {
  var coefficents = Array.fill(3 * 6)(0.0)

  override def add(direction: Vector3, intensity: Vector3): Unit = {
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
      val dst = side * stride
      b.putFloat(dst + 0, coefficents(src + 0).toFloat)
      b.putFloat(dst + 4, coefficents(src + 1).toFloat)
      b.putFloat(dst + 8, coefficents(src + 2).toFloat)
      side += 1
    }
  }

}

