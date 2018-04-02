package core

import java.nio.{ByteBuffer, ByteOrder}

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics

@RunWith(classOf[JUnitRunner])
class ColorTest extends FlatSpec with Matchers {

  val Epsilon = 1e-6f
  implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(Epsilon)

  def assertEqual(lhs: Color, rhs: Color): Unit = {
    withClue(s"$lhs == $rhs") {
      assert(lhs.r === rhs.r)
      assert(lhs.g === rhs.g)
      assert(lhs.b === rhs.b)
      assert(lhs.a === rhs.a)
    }
  }

  "Color.linearToSrgb" should "result in 0.0 -> 0.0" in {
    assert(Color.linearToSrgb(0.0) === 0.0)
  }

  it should "result in 1.0 -> 1.0" in {
    assert(Color.linearToSrgb(1.0) === 1.0)
  }

  "Color.srgbToLinear" should "result in 0.0 -> 0.0" in {
    assert(Color.srgbToLinear(0.0) === 0.0)
  }

  it should "result in 1.0 -> 1.0" in {
    assert(Color.srgbToLinear(1.0) === 1.0)
  }

  "Color.rgb" should "with 0xFFFFFF should be white" in {
    val color = Color.rgb(0xFFFFFF)
    assertEqual(color, Color(1.0, 1.0, 1.0))
  }

  it should "with 0x000000 should be black" in {
    val color = Color.rgb(0x000000)
    assertEqual(color, Color(0.0, 0.0, 0.0))
  }

  it should "round trip with toSrgb8" in {
    val (r, g, b, a) = Color.argb(0x12345678).toSrgb8
    assert(a === 0x12)
    assert(r === 0x34)
    assert(g === 0x56)
    assert(b === 0x78)
  }

  def argbInt32(packed: Int): Int = {
    val a = packed >> 24 & 0xFF
    val r = packed >> 16 & 0xFF
    val g = packed >>  8 & 0xFF
    val b = packed >>  0 & 0xFF
    if (ByteOrder.nativeOrder == ByteOrder.LITTLE_ENDIAN) {
      r | g << 8 | b << 16 | a << 24
    } else {
      a | b << 8 | g << 16 | r << 24
    }
  }

  "toSrgb32Int" should "be all 0x00 on transparent black" in {
    val rgba = Color(0.0, 0.0, 0.0, 0.0).toSrgbInt8
    assert(rgba === argbInt32(0x00000000))
  }

  it should "be all 0xFF on opaque white" in {
    val rgba = Color(1.0, 1.0, 1.0, 1.0).toSrgbInt8
    assert(rgba === argbInt32(0xFFFFFFFF))
  }

  it should "be compatible with toSrgb8" in {
    val r = new java.util.Random(1)
    for (hex <- Iterator.continually(r.nextInt()).take(1000)) {
      val col = Color.argb(hex)
      val (r, g, b, a) = col.toSrgb8
      val packed = col.toSrgbInt8
      assert(r === (packed >>  0 & 0xFF))
      assert(g === (packed >>  8 & 0xFF))
      assert(b === (packed >> 16 & 0xFF))
      assert(a === (packed >> 24 & 0xFF))
    }
  }

  "toSrgb8" should "be all 0x00 on transparent black" in {
    val (r, g, b, a) = Color(0.0, 0.0, 0.0, 0.0).toSrgb8
    assert(a === 0x00)
    assert(r === 0x00)
    assert(g === 0x00)
    assert(b === 0x00)
  }

  it should "be all 0xFF on opaque white" in {
    val (r, g, b, a) = Color(1.0, 1.0, 1.0, 1.0).toSrgb8
    assert(a === 0xFF)
    assert(r === 0xFF)
    assert(g === 0xFF)
    assert(b === 0xFF)
  }

  "toLinear32" should "be all 0x00 on transparent black" in {
    val (r, g, b, a) = Color(0.0, 0.0, 0.0, 0.0).toSrgb8
    assert(a === 0x00)
    assert(r === 0x00)
    assert(g === 0x00)
    assert(b === 0x00)
  }

  it should "be all 0xFF on opaque white" in {
    val (r, g, b, a) = Color(1.0, 1.0, 1.0, 1.0).toSrgb8
    assert(a === 0xFF)
    assert(r === 0xFF)
    assert(g === 0xFF)
    assert(b === 0xFF)
  }

}

