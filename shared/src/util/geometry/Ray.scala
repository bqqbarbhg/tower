package util.geometry

import core._

object Ray {
  def fromUnnormalized(origin: Vector3, unnormalizedDirection: Vector3): Ray = {
    Ray(origin, unnormalizedDirection.normalize)
  }
}

case class Ray(origin: Vector3, direction: Vector3) {

  def intersect(aabb: Aabb): Option[Double] = intersect(aabb, Double.PositiveInfinity)
  def intersect(aabb: Aabb, maxDistance: Double): Option[Double] = {
    val ox = aabb.center.x - origin.x
    val oy = aabb.center.y - origin.y
    val oz = aabb.center.z - origin.z
    val idx = 1.0 / direction.x
    val idy = 1.0 / direction.y
    val idz = 1.0 / direction.z

    var t1 = (ox + aabb.halfSize.x) * idx
    var t2 = (ox - aabb.halfSize.x) * idx

    var tMin = math.min(t1, t2)
    var tMax = math.max(t1, t2)

    t1 = (oy + aabb.halfSize.y) * idy
    t2 = (oy - aabb.halfSize.y) * idy

    tMin = math.max(tMin, math.min(t1, t2))
    tMax = math.min(tMax, math.max(t1, t2))

    t1 = (oz + aabb.halfSize.z) * idz
    t2 = (oz - aabb.halfSize.z) * idz

    tMin = math.max(tMin, math.min(t1, t2))
    tMax = math.min(tMax, math.max(t1, t2))

    if (tMin <= tMax && tMax >= 0.0 && tMin <= maxDistance) {
      Some(math.max(tMin, 0.0))
    } else {
      None
    }
  }

  def intersect(plane: Plane): Option[Double] = intersect(plane, Double.PositiveInfinity)
  def intersect(plane: Plane, maxDistance: Double): Option[Double] = {
    val on = origin dot plane.normal
    val dn = direction dot plane.normal

    // Ray lies on the plane
    if (math.abs(dn) <= Epsilon) {
      return if (math.abs(on) <= Epsilon) Some(0.0) else None
    }

    val t = (plane.d - on) / dn
    if (t >= 0.0 && t <= maxDistance) Some(t) else None
  }

  def intersect(sphere: Sphere): Option[Double] = intersect(sphere, Double.PositiveInfinity)
  def intersect(sphere: Sphere, maxDistance: Double): Option[Double] = {
    val ox = sphere.center.x - origin.x
    val oy = sphere.center.y - origin.y
    val oz = sphere.center.z - origin.z

    val ud = ox*direction.x + oy*direction.y + oz*direction.z
    val uu = ox*ox + oy*oy + oz*oz

    val sqDisc = ud*ud - uu + sphere.radius*sphere.radius
    if (sqDisc < 0.0) return None

    val disc = math.sqrt(sqDisc)
    val t = ud - disc
    if (t >= 0.0 && t <= maxDistance) Some(t) else None
  }

  def point(t: Double): Vector3 = origin + direction * t

  def distanceSquared(point: Vector3): Double = {
    val dx = point.x - origin.x
    val dy = point.y - origin.y
    val dz = point.z - origin.z
    val cx = direction.y*dz - dy*direction.z
    val cy = direction.z*dx - dz*direction.x
    val cz = direction.x*dy - dx*direction.y
    cx*cx + cy*cy + cz*cz
  }

  def distance(point: Vector3): Double = math.sqrt(distanceSquared(point))

}
