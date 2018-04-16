package core

object Quaternion {
  val Identity = Quaternion(0.0, 0.0, 0.0, 1.0)

  def fromAxisAngle(v: Vector3, a: Double): Quaternion = {
    val sin = math.sin(a * 0.5)
    val cos = math.cos(a * 0.5)
    Quaternion(v.x * sin, v.y * sin, v.z * sin, cos)
  }

  def fromAxes(x: Vector3, y: Vector3, z: Vector3): Quaternion = {
    val trace = x.x + y.y + z.z
    if (trace > 0.0) {
      val s = math.sqrt(trace + 1.0) * 2.0
      Quaternion(
        (y.z - z.y) / s,
        (z.x - x.z) / s,
        (x.y - y.x) / s,
        0.25 * s,
      )
    } else if (x.x > y.y && x.x > z.z) {
      val s = math.sqrt(1.0 + x.x - y.y - z.z) * 2.0
      Quaternion(
        0.25 * s,
        (y.x + x.y) / s,
        (z.x + x.z) / s,
        (y.z - z.y) / s,
      )
    } else if (y.y > z.z) {
      val s = math.sqrt(1.0 - x.x + y.y - z.z) * 2.0
      Quaternion(
        (y.x + x.y) / s,
        0.25 * s,
        (z.y + y.z) / s,
        (z.x - x.z) / s,
      )
    } else {
      val s = math.sqrt(1.0 - x.x - y.y + z.z) * 2.0
      Quaternion(
        (z.x + x.z) / s,
        (z.y + y.z) / s,
        0.25 * s,
        (x.y - y.x) / s,
      )
    }
  }

  def lerp(a: Quaternion, b: Quaternion, t: Double): Quaternion = a * (1.0 - t) + b * t
}

case class Quaternion(x: Double, y: Double, z: Double, w: Double) {

  def length: Double = math.sqrt(x*x + y*y + z*z + w*w)
  def normalize: Quaternion = {
    val len = this.length
    assert(math.abs(len) > 0.00001)
    this * (1.0 / length)
  }

  def *(f: Double): Quaternion = Quaternion(x * f, y * f, z * f, w * f)
  def /(f: Double): Quaternion = this * (1.0 / f)
  def +(q: Quaternion): Quaternion = Quaternion(x + q.x, y + q.y, z + q.z, w + q.w)
  def -(q: Quaternion): Quaternion = Quaternion(x - q.x, y - q.y, z - q.z, w - q.w)
  def unary_- = Quaternion(-x, -y, -z, -w)

  def *(q: Quaternion): Quaternion = {
    val rw = w*q.w - x*q.x - y*q.y - z*q.z
    val rx = w*q.x + x*q.w - y*q.z + z*q.y
    val ry = w*q.y + x*q.z + y*q.w - z*q.x
    val rz = w*q.z - x*q.y + y*q.x + z*q.w
    Quaternion(rx, ry, rz, rw)
  }

  def inverse: Quaternion = Quaternion(-x, -y, -z, w)

  def rotate(v: Vector3): Vector3 = {
    val qv = x*v.x + y*v.y + z*v.z
    val qq = x*x + y*y + z*z

    val a = 2.0 * qv
    val b = w*w - qq
    val c = 2.0 * w

    val rx = a*x + b*v.x + c*(y*v.z - v.y*z)
    val ry = a*y + b*v.y + c*(z*v.x - v.z*x)
    val rz = a*z + b*v.z + c*(x*v.y - v.x*y)
    Vector3(rx, ry, rz)
  }

  def rotateInverse(v: Vector3): Vector3 = {
    val x = -this.x
    val y = -this.y
    val z = -this.z

    val qv = x*v.x + y*v.y + z*v.z
    val qq = x*x + y*y + z*z

    val a = 2.0 * qv
    val b = w*w - qq
    val c = 2.0 * w

    val rx = a*x + b*v.x + c*(y*v.z - v.y*z)
    val ry = a*y + b*v.y + c*(z*v.x - v.z*x)
    val rz = a*z + b*v.z + c*(x*v.y - v.x*y)
    Vector3(rx, ry, rz)
  }

}

