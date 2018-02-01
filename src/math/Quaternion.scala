package tower.math

import scala.math.{sqrt, max}
import java.lang.Math.copySign

object Quaternion {
  val Identity = Quaternion(0.0, 0.0, 0.0, 1.0)

  /*
  def fromAxes(x: Vector3, y: Vector3, z: Vector3): Quaternion = {
    val xx = x.x
    val yy = y.y
    val zz = z.z
    val qw = sqrt(max(0.0, 1.0 + xx + yy + zz)) / 2.0
    var qx = sqrt(max(0.0, 1.0 + xx - yy - zz)) / 2.0
    var qy = sqrt(max(0.0, 1.0 - xx + yy - zz)) / 2.0
    var qz = sqrt(max(0.0, 1.0 - xx - yy + zz)) / 2.0
    qx = copySign(qx, z.y - y.z)
    qy = copySign(qy, x.z - z.x)
    qz = copySign(qz, y.x - x.y)
    Quaternion(qx, qy, qz, qw)
  }
  */

  def fromAxes(x: Vector3, y: Vector3, z: Vector3): Quaternion = {
    val trace = x.x + y.y + z.z
    if (trace > 0.0) {
      val s = sqrt(trace + 1.0) * 2.0
      Quaternion(
        (z.y - y.z) / s,
        (x.z - z.x) / s,
        (y.x - x.y) / s,
        0.25 * s,
      )
    } else if (x.x > y.y && x.x > z.z) {
      val s = sqrt(1.0 + x.x - y.y - z.z) * 2.0
      Quaternion(
        0.25 * s,
        (x.y + y.x) / s,
        (x.z + z.x) / s,
        (z.y - y.z) / s,
      )
    } else if (y.y > z.z) {
      val s = sqrt(1.0 - x.x + y.y - z.z) * 2.0
      Quaternion(
        (x.y + y.x) / s,
        0.25 * s,
        (y.z + z.y) / s,
        (x.z - z.x) / s,
      )
    } else {
      val s = sqrt(1.0 - x.x - y.y + z.z) * 2.0
      Quaternion(
        (x.z + z.x) / s,
        (y.z + z.y) / s,
        0.25 * s,
        (y.x - x.y) / s,
      )
    }
  }

  def lerp(a: Quaternion, b: Quaternion, t: Double): Quaternion = a * (1.0 - t) + b * t
}

case class Quaternion(x: Double, y: Double, z: Double, w: Double) {

  def length: Double = math.sqrt(x*x + y*y + z*z + w*w)
  def normalize: Quaternion = {
    val len = this.length
    if (scala.math.abs(len) < 0.0001)
      throw new RuntimeException("Normalizing zero Quaternion")
    else
      this * (1.0 / len)
  }

  def *(f: Double): Quaternion = Quaternion(x * f, y * f, z * f, w * f)
  def /(f: Double): Quaternion = this * (1.0 / f)
  def +(q: Quaternion): Quaternion = Quaternion(x + q.x, y + q.y, z + q.z, w + q.w)
  def -(q: Quaternion): Quaternion = Quaternion(x - q.x, y - q.y, z - q.z, w - q.w)
}
