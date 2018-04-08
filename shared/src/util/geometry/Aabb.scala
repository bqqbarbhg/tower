package util.geometry

import core._

object Aabb {

  def fromMinMax(min: Vector3, max: Vector3): Aabb = {
    val c = (max + min) * 0.5
    val h = (max - min) * 0.5
    Aabb(c, h)
  }

}

final case class Aabb(center: Vector3, halfSize: Vector3) {

  def min: Vector3 = center - halfSize
  def max: Vector3 = center + halfSize

  def intersects(rhs: Aabb): Boolean = {
    val adx = math.abs(center.x - rhs.center.x) - halfSize.x - rhs.halfSize.x
    val ady = math.abs(center.y - rhs.center.y) - halfSize.y - rhs.halfSize.y
    val adz = math.abs(center.z - rhs.center.z) - halfSize.z - rhs.halfSize.z
    !(adx >= 0.0 || ady >= 0.0 || adz >= 0.0)
  }

  def intersects(rhs: Plane): Boolean = rhs.orient(this) == 0

  def intersects(rhs: Sphere): Boolean = {
    val dx = math.max(math.abs(rhs.center.x - center.x) - halfSize.x, 0.0)
    val dy = math.max(math.abs(rhs.center.y - center.y) - halfSize.y, 0.0)
    val dz = math.max(math.abs(rhs.center.z - center.z) - halfSize.z, 0.0)
    val distSq = dx*dx + dy*dy + dz*dz
    distSq <= rhs.radius
  }

  def contains(rhs: Aabb): Boolean = {
    val adx = math.abs(center.x - rhs.center.x) - halfSize.x - rhs.halfSize.x
    val ady = math.abs(center.y - rhs.center.y) - halfSize.y - rhs.halfSize.y
    val adz = math.abs(center.z - rhs.center.z) - halfSize.z - rhs.halfSize.z
    adx <= 0.0 && ady <= 0.0 && adz <= 0.0
  }

  def contains(rhs: Sphere): Boolean = {
    val adx = math.abs(center.x - rhs.center.x) - halfSize.x - rhs.radius
    val ady = math.abs(center.y - rhs.center.y) - halfSize.y - rhs.radius
    val adz = math.abs(center.z - rhs.center.z) - halfSize.z - rhs.radius
    adx <= 0.0 && ady <= 0.0 && adz <= 0.0
  }

}

