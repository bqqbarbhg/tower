package util.geometry

import core._

object Ray {
  def fromUnnormalized(origin: Vector3, unnormalizedDirection: Vector3): Ray = {
    Ray(origin, unnormalizedDirection.normalize)
  }
}

case class Ray(origin: Vector3, direction: Vector3) {

  def intersect(aabb: Aabb): Option[Double] = ???
  def intersect(plane: Plane): Option[Double] = {
    val on = origin dot plane.normal
    val dn = direction dot plane.normal

    // Ray lies on the plane
    if (math.abs(dn) <= Epsilon) {
      return if (math.abs(on) <= Epsilon) Some(0.0) else None
    }

    val t = (plane.d - on) / dn
    if (t >= 0.0) Some(t) else None
  }
  def intersect(sphere: Sphere): Option[Double] = ???

  def point(t: Double): Vector3 = origin + direction * t

}
