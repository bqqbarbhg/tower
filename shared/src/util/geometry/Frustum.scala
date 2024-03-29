package util.geometry

import core._

object Frustum {

  def fromViewProjection(viewProjection: Matrix4): Frustum = {
    val m = viewProjection
    val p = new Array[Plane](6)

    p(0) = Plane.fromUnnormalized(m.m41 + m.m11, m.m42 + m.m12, m.m43 + m.m13, m.m44 + m.m14)
    p(1) = Plane.fromUnnormalized(m.m41 - m.m11, m.m42 - m.m12, m.m43 - m.m13, m.m44 - m.m14)
    p(2) = Plane.fromUnnormalized(m.m41 + m.m21, m.m42 + m.m22, m.m43 + m.m23, m.m44 + m.m24)
    p(3) = Plane.fromUnnormalized(m.m41 - m.m21, m.m42 - m.m22, m.m43 - m.m23, m.m44 - m.m24)
    p(4) = Plane.fromUnnormalized(m.m41 + m.m31, m.m42 + m.m32, m.m43 + m.m33, m.m44 + m.m34)
    p(5) = Plane.fromUnnormalized(m.m41 - m.m31, m.m42 - m.m32, m.m43 - m.m33, m.m44 - m.m34)

    Frustum(p)
  }

}

final case class Frustum(val planes: Array[Plane]) {

  def intersects(rhs: Aabb): Boolean = {
    if (planes(0).orient(rhs) < 0) return false
    if (planes(1).orient(rhs) < 0) return false
    if (planes(2).orient(rhs) < 0) return false
    if (planes(3).orient(rhs) < 0) return false
    if (planes(4).orient(rhs) < 0) return false
    if (planes(5).orient(rhs) < 0) return false
    true
  }

  def intersects(rhs: Sphere): Boolean = {
    if (planes(0).orient(rhs) < 0) return false
    if (planes(1).orient(rhs) < 0) return false
    if (planes(2).orient(rhs) < 0) return false
    if (planes(3).orient(rhs) < 0) return false
    if (planes(4).orient(rhs) < 0) return false
    if (planes(5).orient(rhs) < 0) return false
    true
  }

}

