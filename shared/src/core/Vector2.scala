package core

object Vector2 {
  val Zero = Vector2(0.0, 0.0)
  val One = Vector2(1.0, 1.0)

  def lerp(a: Vector2, b: Vector2, t: Double): Vector2 = a * (1.0 - t) + b * t

  def min(a: Vector2, b: Vector2): Vector2 = Vector2(
    math.min(a.x, b.x),
    math.min(a.y, b.y))

  def max(a: Vector2, b: Vector2): Vector2 = Vector2(
    math.max(a.x, b.x),
    math.max(a.y, b.y))
}

case class Vector2(x: Double, y: Double) {

  def toXz(newY: Double): Vector3 = Vector3(x, newY, y)

  def distanceSquaredTo(a: Vector2): Double = {
    val dx = x - a.x
    val dy = y - a.y
    dx*dx + dy*dy
  }

  def distanceTo(a: Vector2): Double = math.sqrt(distanceSquaredTo(a))

  def lengthSquared: Double = x*x + y*y
  def length: Double = math.sqrt(x*x + y*y)
  def normalize: Vector2 = {
    val len = this.length
    assert(math.abs(len) > 0.00001)
    this * (1.0 / length)
  }
  def normalizeOrZero: Vector2 = {
    val len = this.length
    if (length > 0.0001) {
      this * (1.0 / length)
    } else {
      Vector2.Zero
    }
  }

  def *(f: Double): Vector2 = Vector2(x * f, y * f)
  def /(f: Double): Vector2 = this * (1.0 / f)
  def +(v: Vector2): Vector2 = Vector2(x + v.x, y + v.y)
  def -(v: Vector2): Vector2 = Vector2(x - v.x, y - v.y)
  def unary_- = Vector2(-x, -y)

  // Component-wise operations
  def *@(v: Vector2): Vector2 = Vector2(x * v.x, y * v.y)
  def /@(v: Vector2): Vector2 = Vector2(x / v.x, y / v.y)

  def dot(v: Vector2): Double = x*v.x + y*v.y

  def perpendicular: Vector2 = Vector2(-y, x)
}

