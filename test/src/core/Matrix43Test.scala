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
    Matrix43.worldRot(q1, Vector3(1.0, 2.0, 3.0), Vector3(4.0, 5.0, 6.0))
  }

  val MatB = {
    val x = Vector3(0.3710692,-0.8427673,0.3899371)
    val y = Vector3(-0.9182390,-0.2704403,0.2893082)
    val z = Vector3(-0.1383648,-0.4654088,-0.8742138)
    val q1 = Quaternion.fromAxes(x, y, z)
    Matrix43.worldRot(q1, Vector3(-0.3, 0.5, 0.3), Vector3(0.4, 25.0, -7.0))
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

  "worldRot" should "round trip with Quaternion.fromAxes" in {
    val x = Vector3(0.1333333,-0.6666667,0.7333333)
    val y = Vector3(0.9333333,0.3333333,0.1333333)
    val z = Vector3(-0.3333333,0.6666667,0.6666667)
    val q1 = Quaternion.fromAxes(x, y, z)
    val mat = Matrix43.worldRot(q1, Vector3.One, Vector3.Zero)
    assertEqual(x, mat.right)
    assertEqual(y, mat.up)
    assertEqual(z, mat.forward)
  }

  "determinant" should "be one for rotation matrices" in {
    val x = Vector3(0.1333333,-0.6666667,0.7333333)
    val y = Vector3(0.9333333,0.3333333,0.1333333)
    val z = Vector3(-0.3333333,0.6666667,0.6666667)
    val q1 = Quaternion.fromAxes(x, y, z)
    val mat = Matrix43.worldRot(q1, Vector3.One, Vector3.Zero)
    assert(mat.determinant === 1.0)
  }

  it should "be the cube of the scale factor for uniform scaling" in {
    val mat = Matrix43.worldRot(Quaternion.Identity, Vector3.One * 3.0, Vector3.Zero)
    assert(mat.determinant === 3.0*3.0*3.0)
  }

  "inverse" should "result in identity when multiplied with the original matrix" in {
    val x = Vector3(0.1333333,-0.6666667,0.7333333)
    val y = Vector3(0.9333333,0.3333333,0.1333333)
    val z = Vector3(-0.3333333,0.6666667,0.6666667)
    val q1 = Quaternion.fromAxes(x, y, z)
    val mat = Matrix43.worldRot(q1, Vector3(1.0, 2.5, 7.0), Vector3(-2.0, -5.0, 3.0))

    assertEqualAndAffine(mat * mat.inverse, Matrix43.Identity)
    assertEqualAndAffine(mat.inverse * mat, Matrix43.Identity)
  }

  it should "result in the inverse determinant of the original matrix's determinant" in {
    val x = Vector3(0.1333333,-0.6666667,0.7333333)
    val y = Vector3(0.9333333,0.3333333,0.1333333)
    val z = Vector3(-0.3333333,0.6666667,0.6666667)
    val q1 = Quaternion.fromAxes(x, y, z)
    val mat = Matrix43.worldRot(q1, Vector3(1.0, 2.5, 7.0), Vector3(-2.0, -5.0, 3.0))

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

}

