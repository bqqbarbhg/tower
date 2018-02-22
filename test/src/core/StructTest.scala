package core

import java.nio.{ByteBuffer, ByteOrder}

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics

@RunWith(classOf[JUnitRunner])
class StructTest extends FlatSpec with Matchers {

  object Point extends Struct {
    val X = int
    val Y = int
  }

  object Point3D extends Struct {
    val X = int
    val Y = int
    val Z = int
  }

  object Padded extends Struct {
    val A = int
    val B = long
  }

  "Struct.size" should "return the size of the struct in bytes" in {
    assert(Point.size === 8)
    assert(Point3D.size === 12)
  }

  "Struct.align" should "return the alignment of the largest member" in {
    assert(Point.align === 4)
    assert(Point3D.align === 4)
  }

  "Struct.set" should "store values in memory" in {
    val buf = ByteBuffer.allocateDirect(128).order(ByteOrder.nativeOrder)
    val off = 0

    Point.X.set(buf, off, 123)
    Point.Y.set(buf, off, 456)

    assert(buf.getInt(0) === 123)
    assert(buf.getInt(4) === 456)
  }

  it should "respect base address" in {
    val buf = ByteBuffer.allocateDirect(128).order(ByteOrder.nativeOrder)
    val off = 64

    Point.X.set(buf, off, 123)
    Point.Y.set(buf, off, 456)

    assert(buf.getInt(64) === 123)
    assert(buf.getInt(68) === 456)
  }

  "Struct.get" should "load values from memory" in {
    val buf = ByteBuffer.allocateDirect(128).order(ByteOrder.nativeOrder)
    val off = 0

    buf.putInt(0, 123)
    buf.putInt(4, 456)

    assert(Point.X.get(buf, off) === 123)
    assert(Point.Y.get(buf, off) === 456)
  }

  it should "respect base address" in {
    val buf = ByteBuffer.allocateDirect(128).order(ByteOrder.nativeOrder)
    val off = 64

    buf.putInt(64, 123)
    buf.putInt(68, 456)

    assert(Point.X.get(buf, off) === 123)
    assert(Point.Y.get(buf, off) === 456)
  }

  "Struct" should "add padding to enforce alignment" in {
    assert(Padded.A.offset === 0)
    assert(Padded.B.offset === 8)
    assert(Padded.size === 16)
    assert(Padded.align === 8)
  }

}

