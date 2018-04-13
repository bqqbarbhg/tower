package core

object UnsafeVector2 {
  def Zero: UnsafeVector2 = new UnsafeVector2(0.0, 0.0)
}

class UnsafeVector2(var x: Double, var y: Double) {

  def distanceSquaredTo(a: Vector2): Double = {
    val dx = x - a.x
    val dy = y - a.y
    dx*dx + dy*dy
  }
  def distanceSquaredTo(a: UnsafeVector2): Double = {
    val dx = x - a.x
    val dy = y - a.y
    dx*dx + dy*dy
  }

  def distanceTo(a: Vector2): Double = math.sqrt(distanceSquaredTo(a))
  def distanceTo(a: UnsafeVector2): Double = math.sqrt(distanceSquaredTo(a))

  def lengthSquared: Double = x*x + y*y
  def length: Double = math.sqrt(x*x + y*y)
  def normalize(): Unit = {
    val invLen = 1.0 / length
    x *= invLen
    y *= invLen
  }

  def madd(a: Vector2, b: Double): Unit = { x += a.x * b; y += a.y * b }
  def madd(a: UnsafeVector2, b: Double): Unit = { x += a.x * b; y += a.y * b }

  def %<-(a: Vector2): UnsafeVector2 = { x = a.x; y = a.y; this }
  def %<-(a: UnsafeVector2): UnsafeVector2 = { x = a.x; y = a.y; this }

  def *(f: Double): Unit = { x *= f; y *= f }
  def /(f: Double): Unit = { x /= f; y /= f }
  def +(v: Vector2): Unit = { x += v.x; y += v.y }
  def -(v: Vector2): Unit = { x -= v.x; y -= v.y }
  def +(v: UnsafeVector2): Unit = { x += v.x; y += v.y }
  def -(v: UnsafeVector2): Unit = { x -= v.x; y -= v.y }

  def *=(f: Double): Unit = { x *= f; y *= f }
  def /=(f: Double): Unit = { x /= f; y /= f }
  def +=(v: Vector2): Unit = { x += v.x; y += v.y }
  def -=(v: Vector2): Unit = { x -= v.x; y -= v.y }
  def +=(v: UnsafeVector2): Unit = { x += v.x; y += v.y }
  def -=(v: UnsafeVector2): Unit = { x -= v.x; y -= v.y }

  def dot(v: Vector2): Double = x*v.x + y*v.y
  def dot(v: UnsafeVector2): Double = x*v.x + y*v.y

  def perpendicularTo(v: Vector2): Unit = { x = -v.y; y = v.x }
  def perpendicularTo(v: UnsafeVector2): Unit = {
    val t = v.x
    x = -v.y
    y = t
  }

  def safe: Vector2 = Vector2(x, y)
}

