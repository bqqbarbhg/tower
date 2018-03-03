package core

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics

@RunWith(classOf[JUnitRunner])
class AffineTest extends FlatSpec with Matchers {

  val Epsilon = 1e-4f
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(Epsilon)

  def assertEqual(a: Vector3, b: Vector3): Unit = {
    withClue(s"$a == $b:") {
      assert(a.x === b.x)
      assert(a.y === b.y)
      assert(a.z === b.z)
    }
  }

  def assertEqual(a: Quaternion, b: Quaternion): Unit = {
    withClue(s"$a == $b:") {
      assert(a.x === b.x)
      assert(a.y === b.y)
      assert(a.z === b.z)
      assert(a.w === b.w)
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

  def assertEqual(mat: Matrix43, pos: Vector3, scale: Vector3, x: Vector3, y: Vector3, z: Vector3): Unit = {
    assertEqual(mat.translation, pos)
    assertEqual(mat.right.normalize, x)
    assertEqual(mat.up.normalize, y)
    assertEqual(mat.forward.normalize, z)
    assert(mat.right.length ===  scale.x)
    assert(mat.up.length === scale.y)
    assert(mat.forward.length ===  scale.z)
  }

  "Matrix43.affine" should "work for a translation matrix" in {
    val pos = Vector3(-15.0, -30.0, -45.0)
    val scale = Vector3.One
    val x = Vector3(1.0, 0.0, 0.0).normalize
    val y = Vector3(0.0, 1.0, 0.0).normalize
    val z = Vector3(0.0, 0.0, 1.0).normalize
    val rot = Quaternion.fromAxes(x, y, z)

    val affine = AffineTransform(pos, scale, rot)
    val mat = Matrix43.affine(affine)
    assertEqual(mat, pos, scale, x, y, z)
  }

  it should "work for a rotation matrix" in {
    val pos = Vector3.Zero
    val scale = Vector3.One
    val x = Vector3(1.0, 2.0, 3.0).normalize
    val yt = (Vector3(-4.0, 5.0, -6.0) cross x).normalize
    val z = (yt cross x).normalize
    val y = (z cross x).normalize
    val rot = Quaternion.fromAxes(x, y, z)

    val affine = AffineTransform(pos, scale, rot)
    val mat = Matrix43.affine(affine)
    assertEqual(mat, pos, scale, x, y, z)
  }

  it should "work for a scaling matrix" in {
    val pos = Vector3.Zero
    val scale = Vector3(1.61, 2.72, 3.14)
    val x = Vector3(1.0, 0.0, 0.0).normalize
    val y = Vector3(0.0, 1.0, 0.0).normalize
    val z = Vector3(0.0, 0.0, 1.0).normalize
    val rot = Quaternion.fromAxes(x, y, z)

    val affine = AffineTransform(pos, scale, rot)
    val mat = Matrix43.affine(affine)
    assertEqual(mat, pos, scale, x, y, z)
  }

  it should "work for a mixed matrix" in {
    val pos = Vector3(-15.0, -30.0, -45.0)
    val scale = Vector3(1.61, 2.72, 3.14)
    val x = Vector3(1.0, 2.0, 3.0).normalize
    val yt = (Vector3(-4.0, 5.0, -6.0) cross x).normalize
    val z = (yt cross x).normalize
    val y = (z cross x).normalize
    val rot = Quaternion.fromAxes(x, y, z)

    val affine = AffineTransform(pos, scale, rot)
    val mat = Matrix43.affine(affine)
    assertEqual(mat, pos, scale, x, y, z)
  }

  "Matrix43.toAffine" should "work for a translation matrix" in {
    val pos = Vector3(1.0, 2.0, 3.0)
    val mat = Matrix43.translate(pos)
    val affine = mat.toAffine
    assertEqual(pos, affine.position)
    assertEqual(Vector3.One, affine.scale)
    assertEqual(Quaternion.Identity, affine.rotation)
  }

  it should "work for a scaling matrix" in {
    val scale = Vector3(1.0, 2.0, 3.0)
    val mat = Matrix43.scale(scale)
    val affine = mat.toAffine
    assertEqual(Vector3.Zero, affine.position)
    assertEqual(scale, affine.scale)
    assertEqual(Quaternion.Identity, affine.rotation)
  }

  it should "work for a rotation matrix" in {
    val x = Vector3(1.0, 2.0, 3.0).normalize
    val yt = (Vector3(-4.0, 5.0, -6.0) cross x).normalize
    val z = (yt cross x).normalize
    val y = (z cross x).normalize
    val rot = Quaternion.fromAxes(x, y, z)
    val mat = Matrix43.world(x, y, z)
    val affine = mat.toAffine
    assertEqual(Vector3.Zero, affine.position)
    assertEqual(Vector3.One, affine.scale)
    assertEqual(rot, affine.rotation)
  }

  it should "work for a mixed matrix" in {
    val x = Vector3(1.0, 2.0, 3.0).normalize
    val yt = (Vector3(-4.0, 5.0, -6.0) cross x).normalize
    val z = (yt cross x).normalize
    val y = (z cross x).normalize

    val pos = Vector3(1.0, 2.0, 3.0)
    val scale = Vector3(4.0, 5.0, 6.0)
    val rot = Quaternion.fromAxes(x, y, z)

    val matTrans = Matrix43.translate(pos)
    val matScale = Matrix43.scale(scale)
    val matRot = Matrix43.world(x, y, z)

    val mat = matTrans * matRot * matScale

    val affine = mat.toAffine
    assertEqual(pos, affine.position)
    assertEqual(scale, affine.scale)
    assertEqual(rot, affine.rotation)
  }

}

