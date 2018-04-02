package core

object Vector3 {
  val Zero = Vector3(0.0, 0.0, 0.0)
  val One = Vector3(1.0, 1.0, 1.0)

  def lerp(a: Vector3, b: Vector3, t: Double): Vector3 = a * (1.0 - t) + b * t

  def distanceSquared(a: Vector3, b: Vector3) = {
    val dx = a.x - b.x
    val dy = a.y - b.y
    val dz = a.z - b.z
    dx*dx + dy*dy + dz*dz
  }
}

case class Vector3(x: Double, y: Double, z: Double) {

  def lengthSquared: Double = x*x + y*y + z*z
  def length: Double = math.sqrt(x*x + y*y + z*z)
  def normalize: Vector3 = {
    val len = this.length
    assert(math.abs(len) > 0.00001)
    this * (1.0 / length)
  }

  def *(f: Double): Vector3 = Vector3(x * f, y * f, z * f)
  def /(f: Double): Vector3 = this * (1.0 / f)
  def +(v: Vector3): Vector3 = Vector3(x + v.x, y + v.y, z + v.z)
  def -(v: Vector3): Vector3 = Vector3(x - v.x, y - v.y, z - v.z)
  def unary_- = Vector3(-x, -y, -z)

  // Component-wise operations
  def *@(v: Vector3): Vector3 = Vector3(x * v.x, y * v.y, z * v.z)
  def /@(v: Vector3): Vector3 = Vector3(x / v.x, y / v.y, z / v.z)

  def cross(v: Vector3): Vector3 = Vector3(y*v.z - v.y*z, z*v.x - v.z*x, x*v.y - v.x*y)
  def dot(v: Vector3): Double = x*v.x + y*v.y + z*v.z
}

