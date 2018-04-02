package util.geometry

import core._

object Plane {

  def fromUnnormalized(a: Double, b: Double, c: Double, d: Double): Plane = fromUnnormalized(Vector3(a, b, c), d)

  def fromUnnormalized(normal: Vector3, d: Double): Plane = {
    val length = normal.length
    Plane(normal / length, d / length)
  }

}

final case class Plane(normal: Vector3, d: Double) {
  val anx = math.abs(normal.x)
  val any = math.abs(normal.y)
  val anz = math.abs(normal.z)

  def intersects(rhs: Aabb): Boolean = rhs.intersects(this)

  def intersects(rhs: Plane): Boolean = {
    if (d <= Epsilon && rhs.d <= Epsilon) return true
    if ((normal dot rhs.normal) <= 1.0 - Epsilon) return true
    (d - rhs.d) <= Epsilon
  }

  def intersects(rhs: Sphere): Boolean = this.orient(rhs) == 0


  def orient(rhs: Aabb): Int = {
    val e = rhs.halfSize.x * anx + rhs.halfSize.y * any + rhs.halfSize.z * anz
    val s = (rhs.center dot normal) + d
    if      (s > +e) +1
    else if (s < -e) -1
    else             0
  }

  def orient(rhs: Sphere): Int = {
    val e = rhs.radius
    val s = (rhs.center dot normal) + d
    if      (s > +e) +1
    else if (s < -e) -1
    else             0
  }

}

