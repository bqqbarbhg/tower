package util.geometry

import core._

final case class Sphere(center: Vector3, radius: Double) {

  def intersects(rhs: Aabb): Boolean = rhs.intersects(this)

  def intersects(rhs: Plane): Boolean = rhs.intersects(this)

  def intersects(rhs: Sphere): Boolean = {
    val distSq = Vector3.distanceSquared(rhs.center, center)
    val minDist = radius + rhs.radius
    distSq <= (minDist * minDist)
  }

}

