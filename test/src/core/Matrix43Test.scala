package core

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics

@RunWith(classOf[JUnitRunner])
class Matrix43Test extends FlatSpec with Matchers {

  val Epsilon = 1e-4f
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(Epsilon)

  val MatA = {
    val x = Vector3(0.1333333,-0.6666667,0.7333333)
    val y = Vector3(0.9333333,0.3333333,0.1333333)
    val z = Vector3(-0.3333333,0.6666667,0.6666667)
    val q1 = Quaternion.fromAxes(x, y, z)
    val af = AffineTransform(Vector3(4.0, 5.0, 6.0), Vector3(1.0, 2.0, 3.0), q1)
    Matrix43.affine(af)
  }

  val MatB = {
    val x = Vector3(0.3710692,-0.8427673,0.3899371)
    val y = Vector3(-0.9182390,-0.2704403,0.2893082)
    val z = Vector3(-0.1383648,-0.4654088,-0.8742138)
    val q1 = Quaternion.fromAxes(x, y, z)
    val af = AffineTransform(Vector3(0.4, 25.0, -7.0), Vector3(-0.3, 0.5, 0.3), q1)
    Matrix43.affine(af)
  }

  val MatAB = MatA * MatB

  def assertEqual(a: Vector3, b: Vector3): Unit = {
    withClue(s"$a == $b:") {
      assert(a.x === b.x)
      assert(a.y === b.y)
      assert(a.z === b.z)
    }
  }

  def assertEqualAndAffine(a: Matrix43, b: Matrix43): Unit = {
    withClue(s"$a == $b:") {
      // 4x3 matrices are always affine

      assertEqual(a.right, b.right)
      assertEqual(a.up, b.up)
      assertEqual(a.forward, b.forward)
      assertEqual(a.translation, b.translation)
    }
  }

  "affine" should "round trip with Quaternion.fromAxes" in {
    val x = Vector3(0.1333333,-0.6666667,0.7333333)
    val y = Vector3(0.9333333,0.3333333,0.1333333)
    val z = Vector3(-0.3333333,0.6666667,0.6666667)
    val q1 = Quaternion.fromAxes(x, y, z)
    val mat = Matrix43.affine(Vector3.Zero, Vector3.One, q1)
    assertEqual(x, mat.right)
    assertEqual(y, mat.up)
    assertEqual(z, mat.forward)
  }

  it should "round trip with advanced fixures" in {
    {
      val x = Vector3(0.1333333,0.9333333,-0.3333333)
      val y = Vector3(-0.6666667,0.3333333,0.6666667)
      val z = Vector3(0.7333333,0.1333333,0.6666667)
      val q1 = Quaternion.fromAxes(x, y, z)
      val mat = Matrix43.affine(Vector3.Zero, Vector3.One, q1)
      assertEqual(x, mat.right)
      assertEqual(y, mat.up)
      assertEqual(z, mat.forward)
    }

    {
      val x = Vector3(0.3710692,-0.9182390,-0.1383648)
      val y = Vector3(-0.8427673,-0.2704403,-0.4654088)
      val z = Vector3(0.3899371,0.2893082,-0.8742138)
      val q1 = Quaternion.fromAxes(x, y, z)
      val mat = Matrix43.affine(Vector3.Zero, Vector3.One, q1)
      assertEqual(x, mat.right)
      assertEqual(y, mat.up)
      assertEqual(z, mat.forward)
    }
  }

  "determinant" should "be one for rotation matrices" in {
    val x = Vector3(0.1333333,-0.6666667,0.7333333)
    val y = Vector3(0.9333333,0.3333333,0.1333333)
    val z = Vector3(-0.3333333,0.6666667,0.6666667)
    val q1 = Quaternion.fromAxes(x, y, z)
    val mat = Matrix43.affine(Vector3.Zero, Vector3.One, q1)
    assert(mat.determinant === 1.0)
  }

  it should "be the cube of the scale factor for uniform scaling" in {
    val mat = Matrix43.affine(Vector3.Zero, Vector3.One * 3.0, Quaternion.Identity)
    assert(mat.determinant === 3.0*3.0*3.0)
  }

  "inverse" should "result in identity when multiplied with the original matrix" in {
    val x = Vector3(0.1333333,-0.6666667,0.7333333)
    val y = Vector3(0.9333333,0.3333333,0.1333333)
    val z = Vector3(-0.3333333,0.6666667,0.6666667)
    val q1 = Quaternion.fromAxes(x, y, z)
    val mat = Matrix43.affine(Vector3(-2.0, -5.0, 3.0), Vector3(1.0, 2.5, 7.0), q1)

    assertEqualAndAffine(mat * mat.inverse, Matrix43.Identity)
    assertEqualAndAffine(mat.inverse * mat, Matrix43.Identity)
  }

  it should "result in the inverse determinant of the original matrix's determinant" in {
    val x = Vector3(0.1333333,-0.6666667,0.7333333)
    val y = Vector3(0.9333333,0.3333333,0.1333333)
    val z = Vector3(-0.3333333,0.6666667,0.6666667)
    val q1 = Quaternion.fromAxes(x, y, z)
    val mat = Matrix43.affine(Vector3(-2.0, -5.0, 3.0), Vector3(1.0, 2.5, 7.0), q1)

    assert(mat.inverse.determinant === 1.0 / mat.determinant)
  }

  "unsafeMul" should "give the same result as safe multiply" in {
    val res = new Matrix43()
    res.unsafeMul(MatA, MatB)
    assertEqualAndAffine(res, MatAB)
  }

  "unsafeMulRight" should "give the same result as safe multiply" in {
    val res = MatA.copy
    res.unsafeMulRight(MatB)
    assertEqualAndAffine(res, MatAB)
  }

  "unsafeMulLeft" should "give the same result as safe multiply" in {
    val res = MatB.copy
    res.unsafeMulLeft(MatA)
    assertEqualAndAffine(res, MatAB)
  }

  "appendTranslate" should "give the same result as slow translation" in {

    {
      val offset = Vector3(3.0, 5.0, -7.0)
      val slow = Matrix43.translate(offset) * MatA
      val fast = Matrix43.appendTranslate(MatA, offset)
      assertEqualAndAffine(slow, fast)
    }

    {
      val offset = Vector3(-1.0, 0.0, 0.3)
      val slow = Matrix43.translate(offset) * MatB
      val fast = Matrix43.appendTranslate(MatB, offset)
      assertEqualAndAffine(slow, fast)
    }

    {
      val offset = Vector3(1000.0, -3000.0, 0.0)
      val slow = Matrix43.translate(offset) * Matrix43.Identity
      val fast = Matrix43.appendTranslate(Matrix43.Identity, offset)
      assertEqualAndAffine(slow, fast)
    }

  }

  "unsafeAppendTranslate" should "give the same result as safe appendTranslation" in {

    var fast = new Matrix43.Unsafe()

    {
      val offset = Vector3(3.0, 5.0, -7.0)
      val slow = Matrix43.appendTranslate(MatA, offset)
      Matrix43.unsafeAppendTranslate(fast, MatA, offset)
      assertEqualAndAffine(slow, fast)
    }

    {
      val offset = Vector3(-1.0, 0.0, 0.3)
      val slow = Matrix43.appendTranslate(MatB, offset)
      Matrix43.unsafeAppendTranslate(fast, MatB, offset)
      assertEqualAndAffine(slow, fast)
    }

    {
      val offset = Vector3(1000.0, -3000.0, 0.0)
      val slow = Matrix43.appendTranslate(Matrix43.Identity, offset)
      Matrix43.unsafeAppendTranslate(fast, Matrix43.Identity, offset)
      assertEqualAndAffine(slow, fast)
    }

  }

  "rotate" should "be compatible with axis rotations (X)" in {
    val q = Quaternion.fromAxisAngle(Vector3(1.0, 0.0, 0.0), 1.0)
    val m1 = Matrix43.rotateX(1.0)
    val m2 = Matrix43.rotate(q)
    assertEqualAndAffine(m1, m2)
  }

  it should "be compatible with axis rotations (Y)" in {
    val q = Quaternion.fromAxisAngle(Vector3(0.0, 1.0, 0.0), 1.0)
    val m1 = Matrix43.rotateY(1.0)
    val m2 = Matrix43.rotate(q)
    assertEqualAndAffine(m1, m2)
  }

  it should "be compatible with axis rotations (Z)" in {
    val q = Quaternion.fromAxisAngle(Vector3(0.0, 0.0, 1.0), 1.0)
    val m1 = Matrix43.rotateZ(1.0)
    val m2 = Matrix43.rotate(q)
    assertEqualAndAffine(m1, m2)
  }

}

