package io

import java.nio.{ByteBuffer, ByteOrder}

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class UncheckedBinarySerializationTest extends FlatSpec with Matchers {

  class Test extends SimpleSerializable {
    var a: Int = 0
    var b: Long = 0
    var c: Float = 0
    var d: Double = 0
    var e: Boolean = false

    def visit(v: SimpleVisitor): Unit = {
      a = v.field("a", a)
      b = v.field("b", b)
      c = v.field("c", c)
      d = v.field("d", d)
      e = v.field("e", e)
    }
  }

  class StringContainer extends SimpleSerializable {
    var str: String = ""

    def visit(v: SimpleVisitor): Unit = {
      str = v.field("str", str)
    }
  }

  class RenamedStringContainer extends SimpleSerializable {
    var notStr: String = ""

    def visit(v: SimpleVisitor): Unit = {
      notStr = v.field("notStr", notStr)
    }
  }

  class BadlyNamedIntContainer extends SimpleSerializable {
    var str: Int = 0

    def visit(v: SimpleVisitor): Unit = {
      str = v.field("str", str)
    }
  }

  "UncheckedBinaryWriter" should "blit integer data directly" in {
    val t = new Test()
    t.a = 1; t.b = 2; t.c = 3.0f; t.d = 4.0; t.e = true;
    val buf = ByteBuffer.allocateDirect(128).order(ByteOrder.nativeOrder)
    val wbuf = buf.duplicate.order(ByteOrder.nativeOrder)
    val writer = new UncheckedBinaryWriter(wbuf)
    t.visit(writer)
    assert(buf.getInt() === 1)
    assert(buf.getLong() === 2)
    assert(buf.getFloat() === 3.0f)
    assert(buf.getDouble() === 4.0)
    assert(buf.getInt() === 1)
    assert(buf.position === wbuf.position)
  }

  it should "pad strings to 4 bytes with zero-char" in {
    val sc = new StringContainer()
    sc.str = "Test!"
    val buf = ByteBuffer.allocateDirect(128).order(ByteOrder.nativeOrder)
    val wbuf = buf.duplicate.order(ByteOrder.nativeOrder)
    val writer = new UncheckedBinaryWriter(wbuf)
    sc.visit(writer)
    assert(buf.getInt() === 5)
    assert(buf.getChar() === 'T')
    assert(buf.getChar() === 'e')
    assert(buf.getChar() === 's')
    assert(buf.getChar() === 't')
    assert(buf.getChar() === '!')
    assert(buf.getChar() === '\0')
    assert(buf.position === wbuf.position)
  }

  "UncheckedUtil.fieldHash" should "should be unique for different classes" in {
    val t = new Test()
    val sc = new StringContainer()
    assert(UncheckedUtil.fieldHash(t) !== UncheckedUtil.fieldHash(sc))
  }

  it should "should be unique for different field names" in {
    val t = new RenamedStringContainer()
    val sc = new StringContainer()
    assert(UncheckedUtil.fieldHash(t) !== UncheckedUtil.fieldHash(sc))
  }

  it should "should be unique for different field types" in {
    val t = new BadlyNamedIntContainer()
    val sc = new StringContainer()
    assert(UncheckedUtil.fieldHash(t) !== UncheckedUtil.fieldHash(sc))
  }

}
