package tower.math

import Matrix4._

object Matrix4 {

  /** A singleton "identity" matrix. Do not modify! */
  val Identity = {
    val m = new Matrix4()
    m.m11 = 1.0
    m.m22 = 1.0
    m.m33 = 1.0
    m.m44 = 1.0
    m
  }

  /** Create a new instance of an identity matrix. */
  def makeIdentity = {
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

  def unsafeWorld(r: Matrix4, orientation: Quaternion, scale: Vector3, origin: Vector3 = Vector3.Zero): Unit = {

    val q = orientation
    val s = scale
    val o = origin

    {
      val ww = q.w*q.w
      val xx = q.x*q.x
      val yy = q.y*q.y
      val zz = q.z*q.z

      r.m11 = (+ xx - yy - zz + ww) * s.x
      r.m22 = (- xx + yy - zz + ww) * s.y
      r.m33 = (- xx - yy + zz + ww) * s.z
    }

    {
      val xy = q.x*q.y
      val zw = q.z*q.w
      r.m21 = 2.0 * (xy + zw) * s.y
      r.m12 = 2.0 * (xy - zw) * s.x
    }

    {
      val xz = q.x*q.z
      val yw = q.y*q.w
      r.m31 = 2.0 * (xz - yw) * s.z
      r.m13 = 2.0 * (xz + yw) * s.x
    }

    {
      val yz = q.y*q.z
      val xw = q.x*q.w
      r.m32 = 2.0 * (yz + xw) * s.z
      r.m23 = 2.0 * (yz - xw) * s.y
    }

    r.m14 = r.m11*o.x + r.m12*o.y + r.m13*o.z
    r.m24 = r.m21*o.x + r.m22*o.y + r.m23*o.z
    r.m34 = r.m31*o.x + r.m32*o.y + r.m33*o.z

    r.m44 = 1.0
  }

  def world(orientation: Quaternion, scale: Vector3, origin: Vector3 = Vector3.Zero): Matrix4 = {
    val r = new Matrix4()
    unsafeWorld(r)
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

  /** Copy contents of `m` to this matrix. */
  def unsafeCopy(m: Matrix4): Unit = {
    m11 = m.m11; m12 = m.m12; m13 = m.m13; m14 = m.m14;
    m21 = m.m21; m22 = m.m22; m23 = m.m23; m24 = m.m24;
    m31 = m.m31; m32 = m.m32; m33 = m.m33; m34 = m.m34;
    m41 = m.m41; m42 = m.m42; m43 = m.m43; m44 = m.m44;
  }

  /**
    * Multply matrices `this` * `m` in place and store the result in `this`.
    * Note: `m` may not be equal to `this`!
    */
  def unsafeMulRight(m: Matrix4): Unit = {
    var t1 = m11*m.m11 + m12*m.m21 + m13*m.m31 + m14*m.m41
    var t2 = m11*m.m12 + m12*m.m22 + m13*m.m32 + m14*m.m42
    var t3 = m11*m.m13 + m12*m.m23 + m13*m.m33 + m14*m.m43
    var t4 = m11*m.m14 + m12*m.m24 + m13*m.m34 + m14*m.m44
    m11 = t1; m12 = t2; m13 = t3; m14 = t4;
    t1 = m21*m.m11 + m22*m.m21 + m23*m.m31 + m24*m.m41
    t2 = m21*m.m12 + m22*m.m22 + m23*m.m32 + m24*m.m42
    t3 = m21*m.m13 + m22*m.m23 + m23*m.m33 + m24*m.m43
    t4 = m21*m.m14 + m22*m.m24 + m23*m.m34 + m24*m.m44
    m21 = t1; m22 = t2; m23 = t3; m24 = t4;
    t1 = m31*m.m11 + m32*m.m21 + m33*m.m31 + m34*m.m41
    t2 = m31*m.m12 + m32*m.m22 + m33*m.m32 + m34*m.m42
    t3 = m31*m.m13 + m32*m.m23 + m33*m.m33 + m34*m.m43
    t4 = m31*m.m14 + m32*m.m24 + m33*m.m34 + m34*m.m44
    m31 = t1; m32 = t2; m33 = t3; m34 = t4;
    t1 = m41*m.m11 + m42*m.m21 + m43*m.m31 + m44*m.m41
    t2 = m41*m.m12 + m42*m.m22 + m43*m.m32 + m44*m.m42
    t3 = m41*m.m13 + m42*m.m23 + m43*m.m33 + m44*m.m43
    t4 = m41*m.m14 + m42*m.m24 + m43*m.m34 + m44*m.m44
    m41 = t1; m42 = t2; m43 = t3; m44 = t4;
  }

  /**
    * Multply matrices `m` * `this` in place and store the result in `this`.
    * Note: `m` may not be equal to `this`!
    */
  def unsafeMulLeft(m: Matrix4): Unit = {
    var t1 = m.m11*m11 + m.m12*m21 + m.m13*m31 + m.m14*m41
    var t2 = m.m21*m11 + m.m22*m21 + m.m23*m31 + m.m24*m41
    var t3 = m.m31*m11 + m.m32*m21 + m.m33*m31 + m.m34*m41
    var t4 = m.m41*m11 + m.m42*m21 + m.m43*m31 + m.m44*m41
    m11 = t1; m21 = t2; m31 = t3; m41 = t4;
    t1 = m.m11*m12 + m.m12*m22 + m.m13*m32 + m.m14*m42
    t2 = m.m21*m12 + m.m22*m22 + m.m23*m32 + m.m24*m42
    t3 = m.m31*m12 + m.m32*m22 + m.m33*m32 + m.m34*m42
    t4 = m.m41*m12 + m.m42*m22 + m.m43*m32 + m.m44*m42
    m12 = t1; m22 = t2; m32 = t3; m42 = t4;
    t1 = m.m11*m13 + m.m12*m23 + m.m13*m33 + m.m14*m43
    t2 = m.m21*m13 + m.m22*m23 + m.m23*m33 + m.m24*m43
    t3 = m.m31*m13 + m.m32*m23 + m.m33*m33 + m.m34*m43
    t4 = m.m41*m13 + m.m42*m23 + m.m43*m33 + m.m44*m43
    m13 = t1; m23 = t2; m33 = t3; m43 = t4;
    t1 = m.m11*m14 + m.m12*m24 + m.m13*m34 + m.m14*m44
    t2 = m.m21*m14 + m.m22*m24 + m.m23*m34 + m.m24*m44
    t3 = m.m31*m14 + m.m32*m24 + m.m33*m34 + m.m34*m44
    t4 = m.m41*m14 + m.m42*m24 + m.m43*m34 + m.m44*m44
    m14 = t1; m24 = t2; m34 = t3; m44 = t4;
  }


  /**
    * Multply matrices `a` and `b` in place and store the result in `this`.
    * Note: Neither `a` or `b` may equal to `this`! See `unsafeMulLeft` and `unsafeMulRight`
    */
  def unsafeMul(a: Matrix4, b: Matrix4): Unit = {
    m11 = a.m11*b.m11 + a.m12*b.m21 + a.m13*b.m31 + a.m14*b.m41
    m12 = a.m11*b.m12 + a.m12*b.m22 + a.m13*b.m32 + a.m14*b.m42
    m13 = a.m11*b.m13 + a.m12*b.m23 + a.m13*b.m33 + a.m14*b.m43
    m14 = a.m11*b.m14 + a.m12*b.m24 + a.m13*b.m34 + a.m14*b.m44
    m21 = a.m21*b.m11 + a.m22*b.m21 + a.m23*b.m31 + a.m24*b.m41
    m22 = a.m21*b.m12 + a.m22*b.m22 + a.m23*b.m32 + a.m24*b.m42
    m23 = a.m21*b.m13 + a.m22*b.m23 + a.m23*b.m33 + a.m24*b.m43
    m24 = a.m21*b.m14 + a.m22*b.m24 + a.m23*b.m34 + a.m24*b.m44
    m31 = a.m31*b.m11 + a.m32*b.m21 + a.m33*b.m31 + a.m34*b.m41
    m32 = a.m31*b.m12 + a.m32*b.m22 + a.m33*b.m32 + a.m34*b.m42
    m33 = a.m31*b.m13 + a.m32*b.m23 + a.m33*b.m33 + a.m34*b.m43
    m34 = a.m31*b.m14 + a.m32*b.m24 + a.m33*b.m34 + a.m34*b.m44
    m41 = a.m41*b.m11 + a.m42*b.m21 + a.m43*b.m31 + a.m44*b.m41
    m42 = a.m41*b.m12 + a.m42*b.m22 + a.m43*b.m32 + a.m44*b.m42
    m43 = a.m41*b.m13 + a.m42*b.m23 + a.m43*b.m33 + a.m44*b.m43
    m44 = a.m41*b.m14 + a.m42*b.m24 + a.m43*b.m34 + a.m44*b.m44
  }

  def determinant: Double = {
    + m14*m23*m32*m41 - m13*m24*m32*m41 - m14*m22*m33*m41 + m12*m24*m33*m41
    + m13*m22*m34*m41 - m12*m23*m34*m41 - m14*m23*m31*m42 + m13*m24*m31*m42
    + m14*m21*m33*m42 - m11*m24*m33*m42 - m13*m21*m34*m42 + m11*m23*m34*m42
    + m14*m22*m31*m43 - m12*m24*m31*m43 - m14*m21*m32*m43 + m11*m24*m32*m43
    + m12*m21*m34*m43 - m11*m22*m34*m43 - m13*m22*m31*m44 + m12*m23*m31*m44
    + m13*m21*m32*m44 - m11*m23*m32*m44 - m12*m21*m33*m44 + m11*m22*m33*m44
  }

  def inverse: Matrix4 = {
    val r = new Matrix4
    val invDet = 1.0 / determinant

    r.m11 = (m23*m34*m42 - m24*m33*m42 + m24*m32*m43 - m22*m34*m43 - m23*m32*m44 + m22*m33*m44) * invDet
    r.m12 = (m14*m33*m42 - m13*m34*m42 - m14*m32*m43 + m12*m34*m43 + m13*m32*m44 - m12*m33*m44) * invDet
    r.m13 = (m13*m24*m42 - m14*m23*m42 + m14*m22*m43 - m12*m24*m43 - m13*m22*m44 + m12*m23*m44) * invDet
    r.m14 = (m14*m23*m32 - m13*m24*m32 - m14*m22*m33 + m12*m24*m33 + m13*m22*m34 - m12*m23*m34) * invDet
    r.m21 = (m24*m33*m41 - m23*m34*m41 - m24*m31*m43 + m21*m34*m43 + m23*m31*m44 - m21*m33*m44) * invDet
    r.m22 = (m13*m34*m41 - m14*m33*m41 + m14*m31*m43 - m11*m34*m43 - m13*m31*m44 + m11*m33*m44) * invDet
    r.m23 = (m14*m23*m41 - m13*m24*m41 - m14*m21*m43 + m11*m24*m43 + m13*m21*m44 - m11*m23*m44) * invDet
    r.m24 = (m13*m24*m31 - m14*m23*m31 + m14*m21*m33 - m11*m24*m33 - m13*m21*m34 + m11*m23*m34) * invDet
    r.m31 = (m22*m34*m41 - m24*m32*m41 + m24*m31*m42 - m21*m34*m42 - m22*m31*m44 + m21*m32*m44) * invDet
    r.m32 = (m14*m32*m41 - m12*m34*m41 - m14*m31*m42 + m11*m34*m42 + m12*m31*m44 - m11*m32*m44) * invDet
    r.m33 = (m12*m24*m41 - m14*m22*m41 + m14*m21*m42 - m11*m24*m42 - m12*m21*m44 + m11*m22*m44) * invDet
    r.m34 = (m14*m22*m31 - m12*m24*m31 - m14*m21*m32 + m11*m24*m32 + m12*m21*m34 - m11*m22*m34) * invDet
    r.m41 = (m23*m32*m41 - m22*m33*m41 - m23*m31*m42 + m21*m33*m42 + m22*m31*m43 - m21*m32*m43) * invDet
    r.m42 = (m12*m33*m41 - m13*m32*m41 + m13*m31*m42 - m11*m33*m42 - m12*m31*m43 + m11*m32*m43) * invDet
    r.m43 = (m13*m22*m41 - m12*m23*m41 - m13*m21*m42 + m11*m23*m42 + m12*m21*m43 - m11*m22*m43) * invDet
    r.m44 = (m12*m23*m31 - m13*m22*m31 + m13*m21*m32 - m11*m23*m32 - m12*m21*m33 + m11*m22*m33) * invDet

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
