package core

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics

@RunWith(classOf[JUnitRunner])
class QuaternionTest extends FlatSpec with Matchers {

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

  "length" should "be 1.0 on Identity" in {
    assert(Quaternion.Identity.length === 1.0)
  }

  it should "be 0.0 on zero" in {
    assert(Quaternion(0.0,0.0,0.0,0.0).length == 0.0)
  }

  it should "work for fixtures" in {
    val q1 = Quaternion(1.0, 2.0, 3.0, 4.0)
    assert(q1.length === math.sqrt(30.0))
    val q2 = Quaternion(1.0, 1.0, 1.0, 1.0)
    assert(q2.length === math.sqrt(4.0))
  }

  "normalize" should "result in unit length" in {
    val q1 = Quaternion(1.0, 2.0, 3.0, 4.0).normalize
    assert(q1.length === 1.0)
    val q2 = Quaternion(1.0, 1.0, 1.0, 1.0).normalize
    assert(q2.length === 1.0)
  }

  it should "have throw with zero" in {
    intercept[AssertionError] {
      Quaternion(0.0, 0.0, 0.0, 0.0).normalize
    }
  }

  "fromAxes" should "return Identity for identity axes" in {
    val q = Quaternion.fromAxes(Vector3(1.0, 0.0, 0.0), Vector3(0.0, 1.0, 0.0), Vector3(0.0, 0.0, 1.0))
    assertEqual(q, Quaternion.Identity)
  }

  it should "work for fixtures" in {
    val q1 = Quaternion.fromAxes(Vector3(0.0, 0.0, -1.0), Vector3(0.0, 1.0, 0.0), Vector3(1.0, 0.0, 0.0))
    assertEqual(q1, Quaternion(0.0, 0.7071, 0.0, 0.7071))
  }

  it should "work for advanced fixtures" in {
    val q1 = Quaternion.fromAxes(
      Vector3(0.1333333,0.9333333,-0.3333333),
      Vector3(-0.6666667,0.3333333,0.6666667),
      Vector3(0.7333333,0.1333333,0.6666667),
    )
    assertEqual(q1, Quaternion(0.1825742,0.3651484,0.5477226,0.7302967))

    val q2 = Quaternion.fromAxes(
      Vector3(0.3710692,-0.9182390,-0.1383648),
      Vector3(-0.8427673,-0.2704403,-0.4654088),
      Vector3(0.3899371,0.2893082,-0.8742138),
    )
    assertEqual(q2, Quaternion(0.7930516,-0.5551361,0.0793052,-0.2379155))
  }

  it should "be normalized for orthonormal basis" in {
    val a = Vector3(1.0, 2.0, 3.0).normalize
    val b = (a cross Vector3(-4.0, 0.0, 1.0)).normalize
    val c = (a cross b).normalize
    assert(a.length === 1.0)
    assert(b.length === 1.0)
    assert(c.length === 1.0)
    assert((a dot b) === 0.0)
    assert((b dot c) === 0.0)
    assert((c dot a) === 0.0)

    val q = Quaternion.fromAxes(a, b, c)
    assert(q.length === 1.0)
  }

  it should "work for case where w=0" in {
    val q = Quaternion.fromAxes(
      Vector3( 0.2253735,  0.0000000, -0.9742724),
      Vector3( 0.0000000, -1.0000000,  0.0000000),
      Vector3(-0.9742724,  0.0000000, -0.2253735))
    assertEqual(q, Quaternion(0.7827431, 0.0, -0.6223449, 0.0))
  }

  "fromAxisAngle" should "match Matrix43 conventions" in {
    val q = Quaternion.fromAxisAngle(Vector3.Up, 1.0)
    val m = Matrix43.rotateY(1.0)
    val v = Vector3(1.0, 2.0, 3.0)

    assertEqual(m.projectPoint(v), q.rotate(v))
  }

  "rotate" should "do nothing with identity quaternion" in {
    val v = Vector3(3.0, -3.0, 10.0)
    assertEqual(v, Quaternion.Identity.rotate(v))
  }

  it should "rotate axes correctly" in {
    val x = Vector3(0.3710692,-0.9182390,-0.1383648)
    val y = Vector3(-0.8427673,-0.2704403,-0.4654088)
    val z = Vector3(0.3899371,0.2893082,-0.8742138)
    val q = Quaternion.fromAxes(x, y, z)
    assertEqual(x, q.rotate(Vector3(1.0, 0.0, 0.0)))
    assertEqual(y, q.rotate(Vector3(0.0, 1.0, 0.0)))
    assertEqual(z, q.rotate(Vector3(0.0, 0.0, 1.0)))
  }

}
