package tower.math

object Vector3 {
  def lerp(a: Vector3, b: Vector3, t: Double): Vector3 = a * (1.0 - t) + b * t
}

case class Vector3(x: Double, y: Double, z: Double) {

  def length: Double = math.sqrt(x*x + y*y + z*z)
  def normalize: Vector3 = this * (1.0 / length)

  def *(f: Double): Vector3 = Vector3(x * f, y * f, z * f)
  def /(f: Double): Vector3 = this * (1.0 / f)
  def +(v: Vector3): Vector3 = Vector3(x + v.x, y + v.y, z + v.z)
  def -(v: Vector3): Vector3 = Vector3(x - v.x, y - v.y, z - v.z)

}
