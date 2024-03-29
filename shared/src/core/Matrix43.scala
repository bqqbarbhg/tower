package core

import java.nio.FloatBuffer

import Matrix43._
import com.sun.prism.impl.Disposer.Target
object Matrix43 {

  /** Type alias for mutable matrices */
  type Unsafe = Matrix43

  /** A singleton "identity" matrix. Do not modify! */
  val Identity: Matrix43 = {
    val m = new Matrix43.Unsafe()
    m.m11 = 1.0
    m.m22 = 1.0
    m.m33 = 1.0
    m
  }

  /** Create a new instance of an identity matrix. */
  def unsafeIdentity: Matrix43.Unsafe = {
    val m = new Matrix43.Unsafe()
    m.m11 = 1.0
    m.m22 = 1.0
    m.m33 = 1.0
    m
  }

  /** Rotation around the X axis (angle is in radians) */
  def rotateX(angle: Double): Matrix43 = {
    val m = Matrix43.unsafeIdentity
    val c = math.cos(angle)
    val s = math.sin(angle)
    m.m22 = c
    m.m23 = -s
    m.m32 = s
    m.m33 = c
    m
  }

  /** Rotation around the Y axis (angle is in radians) */
  def rotateY(angle: Double): Matrix43 = {
    val m = Matrix43.unsafeIdentity
    val c = math.cos(angle)
    val s = math.sin(angle)
    m.m11 = c
    m.m13 = s
    m.m31 = -s
    m.m33 = c
    m
  }

  /** Rotation around the Z axis (angle is in radians) */
  def rotateZ(angle: Double): Matrix43 = {
    val m = Matrix43.unsafeIdentity
    val c = math.cos(angle)
    val s = math.sin(angle)
    m.m11 = c
    m.m12 = -s
    m.m21 = s
    m.m22 = c
    m
  }

  /** Uniform scale */
  def scale(amount: Double): Matrix43 = {
    val m = new Matrix43.Unsafe()
    m.m11 = amount
    m.m22 = amount
    m.m33 = amount
    m
  }

  /** Non-uniform scale */
  def scale(amount: Vector3): Matrix43 = {
    val m = new Matrix43.Unsafe()
    m.m11 = amount.x
    m.m22 = amount.y
    m.m33 = amount.z
    m
  }

  /** Translation */
  def translate(amount: Vector3): Matrix43 = {
    val m = Matrix43.unsafeIdentity
    m.m14 = amount.x
    m.m24 = amount.y
    m.m34 = amount.z
    m
  }

  /** Translation */
  def translate(x: Double, y: Double, z: Double): Matrix43 = {
    val m = Matrix43.unsafeIdentity
    m.m14 = x
    m.m24 = y
    m.m34 = z
    m
  }

  /** Rotation */
  def rotate(rotation: Quaternion): Matrix43 = {
    val x = rotation.x
    val y = rotation.y
    val z = rotation.z
    val w = rotation.w

    val m = new Matrix43.Unsafe()
    m.m11 = 1.0 - 2.0*y*y - 2.0*z*z
    m.m12 = 2.0*x*y - 2.0*z*w
    m.m13 = 2.0*x*z + 2.0*y*w
    m.m21 = 2.0*x*y + 2.0*z*w
    m.m22 = 1.0 - 2.0*x*x - 2.0*z*z
    m.m23 = 2.0*y*z - 2.0*x*w
    m.m31 = 2.0*x*z - 2.0*y*w
    m.m32 = 2.0*y*z + 2.0*x*w
    m.m33 = 1.0 - 2.0*x*x - 2.0*y*y
    m
  }

  /**
    * Unsafe version of `worldRot()`: Mutates argument r!
    */
  def unsafeAffine(r: Matrix43, affine: AffineTransform): Unit = {
    val q = affine.rotation
    val s = affine.scale
    val o = affine.position

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
      r.m21 = 2.0 * (xy + zw) * s.x
      r.m12 = 2.0 * (xy - zw) * s.y
    }

    {
      val xz = q.x*q.z
      val yw = q.y*q.w
      r.m31 = 2.0 * (xz - yw) * s.x
      r.m13 = 2.0 * (xz + yw) * s.z
    }

    {
      val yz = q.y*q.z
      val xw = q.x*q.w
      r.m32 = 2.0 * (yz + xw) * s.y
      r.m23 = 2.0 * (yz - xw) * s.z
    }

    r.m14 = o.x
    r.m24 = o.y
    r.m34 = o.z
  }

  /**
    * Unsafe version of `appendTranslate()`
    * Apply translation to `a` and store result unsafely in mutable `r`.
    *
    * Equivalent to: `r = Matrix43.translate(translation) * a`
    */
  def unsafeAppendTranslate(r: Matrix43, a: Matrix43, translation: Vector3): Unit = {
    r.m11 = a.m11; r.m12 = a.m12; r.m13 = a.m13
    r.m21 = a.m21; r.m22 = a.m22; r.m23 = a.m23
    r.m31 = a.m31; r.m32 = a.m32; r.m33 = a.m33

    r.m14 = a.m14 + translation.x
    r.m24 = a.m24 + translation.y
    r.m34 = a.m34 + translation.z
  }

  /**
    * Apply translation to `a`.
    *
    * Equivalent to: `Matrix43.translate(translation) * a`
    */
  def appendTranslate(a: Matrix43, translation: Vector3): Matrix43 = {
    val r = new Matrix43.Unsafe()
    unsafeAppendTranslate(r, a, translation)
    r
  }

  /**
    * Create a transform matrix from scale, rotation, and translation in that order.
    */
  def affine(affine: AffineTransform): Matrix43 = {
    val r = new Matrix43.Unsafe()
    unsafeAffine(r, affine)
    r
  }

  /**
    * Create a transform matrix from scale, rotation, and translation in that order.
    */
  def affine(position: Vector3, scale: Vector3, rotation: Quaternion): Matrix43 = {
    affine(AffineTransform(position, scale, rotation))
  }

  /** Create a world transform having axes `x`, `y`, `z` and translation `origin`. */
  def world(x: Vector3, y: Vector3, z: Vector3, origin: Vector3 = Vector3.Zero): Matrix43 = {
    val r = new Matrix43.Unsafe()
    r.m11 = x.x
    r.m21 = x.y
    r.m31 = x.z
    r.m12 = y.x
    r.m22 = y.y
    r.m32 = y.z
    r.m13 = z.x
    r.m23 = z.y
    r.m33 = z.z
    r.m14 = origin.x
    r.m24 = origin.y
    r.m34 = origin.z
    r
  }

  /** Create the inverse of a world transform having axes `x`, `y`, `z` and
    * translation `origin`. */
  def inverseWorld(x: Vector3, y: Vector3, z: Vector3, origin: Vector3 = Vector3.Zero): Matrix43 = {
    val r = new Matrix43.Unsafe()
    r.m11 = x.x
    r.m12 = x.y
    r.m13 = x.z
    r.m14 = -origin dot x
    r.m21 = y.x
    r.m22 = y.y
    r.m23 = y.z
    r.m24 = -origin dot y
    r.m31 = z.x
    r.m32 = z.y
    r.m33 = z.z
    r.m34 = -origin dot z
    r
  }

  def unsafeWorld(r: Matrix43, translation: Vector3, rotation: Quaternion, scale: Vector3): Unit = {
    val x = rotation.x
    val y = rotation.y
    val z = rotation.z
    val w = rotation.w

    r.m11 = (1.0 - 2.0*y*y - 2.0*z*z) * scale.x
    r.m12 = (2.0*x*y - 2.0*z*w) * scale.y
    r.m13 = (2.0*x*z + 2.0*y*w) * scale.z
    r.m21 = (2.0*x*y + 2.0*z*w) * scale.x
    r.m22 = (1.0 - 2.0*x*x - 2.0*z*z) * scale.y
    r.m23 = (2.0*y*z - 2.0*x*w) * scale.z
    r.m31 = (2.0*x*z - 2.0*y*w) * scale.x
    r.m32 = (2.0*y*z + 2.0*x*w) * scale.y
    r.m33 = (1.0 - 2.0*x*x - 2.0*y*y) * scale.z
    r.m14 = translation.x
    r.m24 = translation.y
    r.m34 = translation.z
  }

  def world(translation: Vector3, rotation: Quaternion, scale: Vector3): Matrix43 = {
    val r = new Matrix43.Unsafe()
    unsafeWorld(r, translation, rotation, scale)
    r
  }

  def unsafeWorld(r: Matrix43, translation: Vector3, rotation: Quaternion): Unit = {
    val x = rotation.x
    val y = rotation.y
    val z = rotation.z
    val w = rotation.w

    r.m11 = 1.0 - 2.0*y*y - 2.0*z*z
    r.m12 = 2.0*x*y - 2.0*z*w
    r.m13 = 2.0*x*z + 2.0*y*w
    r.m21 = 2.0*x*y + 2.0*z*w
    r.m22 = 1.0 - 2.0*x*x - 2.0*z*z
    r.m23 = 2.0*y*z - 2.0*x*w
    r.m31 = 2.0*x*z - 2.0*y*w
    r.m32 = 2.0*y*z + 2.0*x*w
    r.m33 = 1.0 - 2.0*x*x - 2.0*y*y
    r.m14 = translation.x
    r.m24 = translation.y
    r.m34 = translation.z
  }

  def world(translation: Vector3, rotation: Quaternion): Matrix43 = {
    val r = new Matrix43.Unsafe()
    unsafeWorld(r, translation, rotation)
    r
  }

  /** Create a view-matrix looking from `eye` to _direction_ `dir` (note: not a
    * target as in many APIs) */
  def look(eye: Vector3, dir: Vector3, up: Vector3 = Vector3(0.0, 1.0, 0.0)): Matrix43 = {
    val dir2 = dir.normalize
    val right = (dir2 cross up).normalize
    val up2 = right cross dir2
    inverseWorld(right, up2, dir2, eye)
  }
}

class Matrix43 {

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

  def *(m: Matrix43): Matrix43 = {
    val r = new Matrix43()

    r.m11 = m11*m.m11 + m12*m.m21 + m13*m.m31
    r.m12 = m11*m.m12 + m12*m.m22 + m13*m.m32
    r.m13 = m11*m.m13 + m12*m.m23 + m13*m.m33
    r.m14 = m11*m.m14 + m12*m.m24 + m13*m.m34 + m14
    r.m21 = m21*m.m11 + m22*m.m21 + m23*m.m31
    r.m22 = m21*m.m12 + m22*m.m22 + m23*m.m32
    r.m23 = m21*m.m13 + m22*m.m23 + m23*m.m33
    r.m24 = m21*m.m14 + m22*m.m24 + m23*m.m34 + m24
    r.m31 = m31*m.m11 + m32*m.m21 + m33*m.m31
    r.m32 = m31*m.m12 + m32*m.m22 + m33*m.m32
    r.m33 = m31*m.m13 + m32*m.m23 + m33*m.m33
    r.m34 = m31*m.m14 + m32*m.m24 + m33*m.m34 + m34

    r
  }

  def copy: Matrix43 = {
    val r = new Matrix43()
    r.unsafeCopy(this)
    r
  }

  /** Copy contents of `m` to this matrix. */
  def unsafeCopy(m: Matrix43): Unit = {
    m11 = m.m11; m12 = m.m12; m13 = m.m13; m14 = m.m14;
    m21 = m.m21; m22 = m.m22; m23 = m.m23; m24 = m.m24;
    m31 = m.m31; m32 = m.m32; m33 = m.m33; m34 = m.m34;
  }

  /**
    * Multply matrices `this` * `m` in place and store the result in `this`.
    * Note: `m` may not be equal to `this`!
    */
  def unsafeMulRight(m: Matrix43): Unit = {
    var t1 = m11*m.m11 + m12*m.m21 + m13*m.m31
    var t2 = m11*m.m12 + m12*m.m22 + m13*m.m32
    var t3 = m11*m.m13 + m12*m.m23 + m13*m.m33
    var t4 = m11*m.m14 + m12*m.m24 + m13*m.m34 + m14
    m11 = t1; m12 = t2; m13 = t3; m14 = t4;
    t1 = m21*m.m11 + m22*m.m21 + m23*m.m31
    t2 = m21*m.m12 + m22*m.m22 + m23*m.m32
    t3 = m21*m.m13 + m22*m.m23 + m23*m.m33
    t4 = m21*m.m14 + m22*m.m24 + m23*m.m34 + m24
    m21 = t1; m22 = t2; m23 = t3; m24 = t4;
    t1 = m31*m.m11 + m32*m.m21 + m33*m.m31
    t2 = m31*m.m12 + m32*m.m22 + m33*m.m32
    t3 = m31*m.m13 + m32*m.m23 + m33*m.m33
    t4 = m31*m.m14 + m32*m.m24 + m33*m.m34 + m34
    m31 = t1; m32 = t2; m33 = t3; m34 = t4;
  }

  /**
    * Multply matrices `m` * `this` in place and store the result in `this`.
    * Note: `m` may not be equal to `this`!
    */
  def unsafeMulLeft(m: Matrix43): Unit = {
    var t1 = m.m11*m11 + m.m12*m21 + m.m13*m31
    var t2 = m.m21*m11 + m.m22*m21 + m.m23*m31
    var t3 = m.m31*m11 + m.m32*m21 + m.m33*m31
    m11 = t1; m21 = t2; m31 = t3;
    t1 = m.m11*m12 + m.m12*m22 + m.m13*m32
    t2 = m.m21*m12 + m.m22*m22 + m.m23*m32
    t3 = m.m31*m12 + m.m32*m22 + m.m33*m32
    m12 = t1; m22 = t2; m32 = t3;
    t1 = m.m11*m13 + m.m12*m23 + m.m13*m33
    t2 = m.m21*m13 + m.m22*m23 + m.m23*m33
    t3 = m.m31*m13 + m.m32*m23 + m.m33*m33
    m13 = t1; m23 = t2; m33 = t3;
    t1 = m.m11*m14 + m.m12*m24 + m.m13*m34 + m.m14
    t2 = m.m21*m14 + m.m22*m24 + m.m23*m34 + m.m24
    t3 = m.m31*m14 + m.m32*m24 + m.m33*m34 + m.m34
    m14 = t1; m24 = t2; m34 = t3;
  }


  /**
    * Multply matrices `a` and `b` in place and store the result in `this`.
    * Note: Neither `a` or `b` may equal to `this`! See `unsafeMulLeft` and `unsafeMulRight`
    */
  def unsafeMul(a: Matrix43, b: Matrix43): Unit = {
    m11 = a.m11*b.m11 + a.m12*b.m21 + a.m13*b.m31
    m12 = a.m11*b.m12 + a.m12*b.m22 + a.m13*b.m32
    m13 = a.m11*b.m13 + a.m12*b.m23 + a.m13*b.m33
    m14 = a.m11*b.m14 + a.m12*b.m24 + a.m13*b.m34 + a.m14
    m21 = a.m21*b.m11 + a.m22*b.m21 + a.m23*b.m31
    m22 = a.m21*b.m12 + a.m22*b.m22 + a.m23*b.m32
    m23 = a.m21*b.m13 + a.m22*b.m23 + a.m23*b.m33
    m24 = a.m21*b.m14 + a.m22*b.m24 + a.m23*b.m34 + a.m24
    m31 = a.m31*b.m11 + a.m32*b.m21 + a.m33*b.m31
    m32 = a.m31*b.m12 + a.m32*b.m22 + a.m33*b.m32
    m33 = a.m31*b.m13 + a.m32*b.m23 + a.m33*b.m33
    m34 = a.m31*b.m14 + a.m32*b.m24 + a.m33*b.m34 + a.m34
  }

  def determinant: Double = (
    - m13*m22*m31 + m12*m23*m31
      + m13*m21*m32 - m11*m23*m32
      - m12*m21*m33 + m11*m22*m33
    )

  def inverse: Matrix43 = {
    val r = new Matrix43
    val invDet = 1.0 / determinant

    r.m11 = (- m23*m32*1 + m22*m33*1) * invDet
    r.m12 = (+ m13*m32*1 - m12*m33*1) * invDet
    r.m13 = (- m13*m22*1 + m12*m23*1) * invDet
    r.m14 = (m14*m23*m32 - m13*m24*m32 - m14*m22*m33 + m12*m24*m33 + m13*m22*m34 - m12*m23*m34) * invDet
    r.m21 = (+ m23*m31*1 - m21*m33*1) * invDet
    r.m22 = (- m13*m31*1 + m11*m33*1) * invDet
    r.m23 = (+ m13*m21*1 - m11*m23*1) * invDet
    r.m24 = (m13*m24*m31 - m14*m23*m31 + m14*m21*m33 - m11*m24*m33 - m13*m21*m34 + m11*m23*m34) * invDet
    r.m31 = (- m22*m31*1 + m21*m32*1) * invDet
    r.m32 = (+ m12*m31*1 - m11*m32*1) * invDet
    r.m33 = (- m12*m21*1 + m11*m22*1) * invDet
    r.m34 = (m14*m22*m31 - m12*m24*m31 - m14*m21*m32 + m11*m24*m32 + m12*m21*m34 - m11*m22*m34) * invDet

    r
  }

  /** Store as a 4x4 matrix */
  def store44(array: Array[Float], offset: Int = 0): Unit = {
    array(offset + 0)  = m11.toFloat
    array(offset + 1)  = m21.toFloat
    array(offset + 2)  = m31.toFloat
    array(offset + 3)  = 0.0f
    array(offset + 4)  = m12.toFloat
    array(offset + 5)  = m22.toFloat
    array(offset + 6)  = m32.toFloat
    array(offset + 7)  = 0.0f
    array(offset + 8)  = m13.toFloat
    array(offset + 9)  = m23.toFloat
    array(offset + 10) = m33.toFloat
    array(offset + 11) = 0.0f
    array(offset + 12) = m14.toFloat
    array(offset + 13) = m24.toFloat
    array(offset + 14) = m34.toFloat
    array(offset + 15) = 1.0f
  }

  /** Store as a 4x4 matrix */
  def store44(buf: FloatBuffer): Unit = {
    buf.put(0, m11.toFloat)
    buf.put(1, m21.toFloat)
    buf.put(2, m31.toFloat)
    buf.put(3, 0.0f)
    buf.put(4, m12.toFloat)
    buf.put(5, m22.toFloat)
    buf.put(6, m32.toFloat)
    buf.put(7, 0.0f)
    buf.put(8, m13.toFloat)
    buf.put(9, m23.toFloat)
    buf.put(10, m33.toFloat)
    buf.put(11, 0.0f)
    buf.put(12, m14.toFloat)
    buf.put(13, m24.toFloat)
    buf.put(14, m34.toFloat)
    buf.put(15, 1.0f)
  }

  /** Store as a 4x4 matrix */
  def store44(buf: FloatBuffer, offset: Int): Unit = {
    buf.put(offset + 0, m11.toFloat)
    buf.put(offset + 1, m21.toFloat)
    buf.put(offset + 2, m31.toFloat)
    buf.put(offset + 3, 0.0f)
    buf.put(offset + 4, m12.toFloat)
    buf.put(offset + 5, m22.toFloat)
    buf.put(offset + 6, m32.toFloat)
    buf.put(offset + 7, 0.0f)
    buf.put(offset + 8, m13.toFloat)
    buf.put(offset + 9, m23.toFloat)
    buf.put(offset + 10, m33.toFloat)
    buf.put(offset + 11, 0.0f)
    buf.put(offset + 12, m14.toFloat)
    buf.put(offset + 13, m24.toFloat)
    buf.put(offset + 14, m34.toFloat)
    buf.put(offset + 15, 1.0f)
  }

  /** Decompose the matrix into an affine transform */
  def toAffine: AffineTransform = {
    val pos = Vector3(m14, m24, m34)
    val scale = Vector3(
      math.sqrt(m11*m11 + m21*m21 + m31*m31),
      math.sqrt(m12*m12 + m22*m22 + m32*m32),
      math.sqrt(m13*m13 + m23*m23 + m33*m33))
    val x = Vector3(m11, m21, m31) / scale.x
    val y = Vector3(m12, m22, m32) / scale.y
    val z = Vector3(m13, m23, m33) / scale.z
    val rot = Quaternion.fromAxes(x, y, z)
    AffineTransform(pos, scale, rot)
  }

  /** Project a 4D vector with XYZ from `v` and W=1 */
  def projectPoint(v: Vector3): Vector3 = {
    val x = m11*v.x + m12*v.y + m13*v.z + m14
    val y = m21*v.x + m22*v.y + m23*v.z + m24
    val z = m31*v.x + m32*v.y + m33*v.z + m34
    Vector3(x, y, z)
  }

  def right: Vector3 = Vector3(m11, m21, m31)
  def up: Vector3 = Vector3(m12, m22, m32)
  def forward: Vector3 = Vector3(m13, m23, m33)
  def translation: Vector3 = Vector3(m14, m24, m34)
}

