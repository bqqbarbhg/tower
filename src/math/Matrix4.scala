package tower.math

import Matrix4._

object Matrix4 {

  val Identity = {
    val m = new Matrix4()
    m.m11 = 1.0
    m.m22 = 1.0
    m.m33 = 1.0
    m.m44 = 1.0
    m
  }

  def perspective(aspect: Double, fov: Double, near: Double, far: Double) = {
    val r = new Matrix4()

    r.m11 = 1.0 / (aspect * scala.math.tan(fov / 2.0))
    r.m22 = 1.0 / (scala.math.tan(fov / 2.0))
    r.m33 = (far + near) / (far - near)
    r.m34 = -(2.0 * near * far) / (far - near)
    r.m43 = 1.0

    r
  }

  def rotateX(angle: Double): Matrix4 = {
    val m = Matrix4.Identity
    val c = math.cos(angle)
    val s = math.sin(angle)
    m.m22 = c
    m.m23 = s
    m.m32 = -s
    m.m33 = c
    m
  }

  def rotateY(angle: Double): Matrix4 = {
    val m = Matrix4.Identity
    val c = math.cos(angle)
    val s = math.sin(angle)
    m.m11 = c
    m.m13 = s
    m.m31 = -s
    m.m33 = c
    m
  }

  def world(forward: Vector3, right: Vector3, up: Vector3, origin: Vector3 = Vector3.Zero): Matrix4 = {
    val r = new Matrix4()
    r.m11 = right.x
    r.m12 = right.y
    r.m13 = right.z
    r.m14 = origin dot right
    r.m21 = up.x
    r.m22 = up.y
    r.m23 = up.z
    r.m24 = origin dot up
    r.m31 = forward.x
    r.m32 = forward.y
    r.m33 = forward.z
    r.m34 = origin dot forward
    r.m44 = 1.0
    r
  }

  def look(eye: Vector3, dir: Vector3, up: Vector3 = Vector3(0.0, 1.0, 0.0)): Matrix4 = {
    val dir2 = dir.normalize
    val right = (up cross dir2).normalize
    val up2 = dir2 cross right
    world(dir2, right, up2, -eye)
  }
}

class Matrix4 {

  var m11: Double = 0.0
  var m12: Double = 0.0
  var m13: Double = 0.0
  var m14: Double = 0.0
  var m21: Double = 0.0
  var m22: Double = 0.0
  var m23: Double = 0.0
  var m24: Double = 0.0
  var m31: Double = 0.0
  var m32: Double = 0.0
  var m33: Double = 0.0
  var m34: Double = 0.0
  var m41: Double = 0.0
  var m42: Double = 0.0
  var m43: Double = 0.0
  var m44: Double = 0.0

  def *(m: Matrix4): Matrix4 = {
    val r = new Matrix4()

    r.m11 = m11*m.m11 + m12*m.m21 + m13*m.m31 + m14*m.m41
    r.m12 = m11*m.m12 + m12*m.m22 + m13*m.m32 + m14*m.m42
    r.m13 = m11*m.m13 + m12*m.m23 + m13*m.m33 + m14*m.m43
    r.m14 = m11*m.m14 + m12*m.m24 + m13*m.m34 + m14*m.m44
    r.m21 = m21*m.m11 + m22*m.m21 + m23*m.m31 + m24*m.m41
    r.m22 = m21*m.m12 + m22*m.m22 + m23*m.m32 + m24*m.m42
    r.m23 = m21*m.m13 + m22*m.m23 + m23*m.m33 + m24*m.m43
    r.m24 = m21*m.m14 + m22*m.m24 + m23*m.m34 + m24*m.m44
    r.m31 = m31*m.m11 + m32*m.m21 + m33*m.m31 + m34*m.m41
    r.m32 = m31*m.m12 + m32*m.m22 + m33*m.m32 + m34*m.m42
    r.m33 = m31*m.m13 + m32*m.m23 + m33*m.m33 + m34*m.m43
    r.m34 = m31*m.m14 + m32*m.m24 + m33*m.m34 + m34*m.m44
    r.m41 = m41*m.m11 + m42*m.m21 + m43*m.m31 + m44*m.m41
    r.m42 = m41*m.m12 + m42*m.m22 + m43*m.m32 + m44*m.m42
    r.m43 = m41*m.m13 + m42*m.m23 + m43*m.m33 + m44*m.m43
    r.m44 = m41*m.m14 + m42*m.m24 + m43*m.m34 + m44*m.m44

    r
  }

  def store(array: Array[Float]): Unit = {
    array(0)  = m11.toFloat
    array(1)  = m21.toFloat
    array(2)  = m31.toFloat
    array(3)  = m41.toFloat
    array(4)  = m12.toFloat
    array(5)  = m22.toFloat
    array(6)  = m32.toFloat
    array(7)  = m42.toFloat
    array(8)  = m13.toFloat
    array(9)  = m23.toFloat
    array(10) = m33.toFloat
    array(11) = m43.toFloat
    array(12) = m14.toFloat
    array(13) = m24.toFloat
    array(14) = m34.toFloat
    array(15) = m44.toFloat
  }

}
