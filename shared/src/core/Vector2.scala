package core

object Vector2 {
  val Zero = Vector2(0.0, 0.0)

  def lerp(a: Vector2, b: Vector2, t: Double): Vector2 = a * (1.0 - t) + b * t
}

case class Vector2(x: Double, y: Double) {

  def length: Double = math.sqrt(x*x + y*y)
  def normalize: Vector2 = {
    val len = this.length
    assert(math.abs(len) > 0.00001)
    this * (1.0 / length)
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

