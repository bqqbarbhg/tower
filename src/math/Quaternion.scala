package tower.math

object Quaternion {
  val Identity = Quaternion(0.0, 0.0, 0.0, 1.0)

  def lerp(a: Quaternion, b: Quaternion, t: Double): Quaternion = a * (1.0 - t) + b * t
}

case class Quaternion(x: Double, y: Double, z: Double, w: Double) {

  def length: Double = math.sqrt(x*x + y*y + z*z + w*w)
  def normalize: Quaternion = this * (1.0 / length)

  def *(f: Double): Quaternion = Quaternion(x * f, y * f, z * f, w * f)
  def /(f: Double): Quaternion = this * (1.0 / f)
  def +(q: Quaternion): Quaternion = Quaternion(x + q.x, y + q.y, z + q.z, w + q.w)
  def -(q: Quaternion): Quaternion = Quaternion(x - q.x, y - q.y, z - q.z, w - q.w)
}
