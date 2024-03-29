package tower.math

object Vector3 {
  val Zero = Vector3(0.0, 0.0, 0.0)
  val One = Vector3(1.0, 1.0, 1.0)

  def lerp(a: Vector3, b: Vector3, t: Double): Vector3 = a * (1.0 - t) + b * t
}

case class Vector3(x: Double, y: Double, z: Double) {

  def length: Double = math.sqrt(x*x + y*y + z*z)
  def normalize: Vector3 = {
    val len = length
    if (scala.math.abs(len) < 0.0001)
      throw new RuntimeException("Normalizing zero Vector3")
    else
      this * (1.0 / len)
  }

  def *(f: Double): Vector3 = Vector3(x * f, y * f, z * f)
  def /(f: Double): Vector3 = this * (1.0 / f)
  def +(v: Vector3): Vector3 = Vector3(x + v.x, y + v.y, z + v.z)
  def -(v: Vector3): Vector3 = Vector3(x - v.x, y - v.y, z - v.z)
  def unary_- = Vector3(-x, -y, -z)

  def cross(v: Vector3): Vector3 = Vector3(y*v.z - v.y*z, z*v.x - v.z*x, x*v.y - v.x*y)
  def dot(v: Vector3): Double = x*v.x + y*v.y + z*v.z

}
