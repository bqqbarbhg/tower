package io

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics
import io.SimpleSerialization._

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class SimpleSerializationTest extends FlatSpec with Matchers {

  class Test extends SimpleSerializable {
    var foo = 0
    var bar = ""

    def visit(v: SimpleVisitor): Unit = {
      foo = v.field("foo", foo)
      bar = v.field("bar", bar)
    }
  }

  class Nested extends SimpleSerializable {
    var test = new Test()
    var flag = false

    def visit(v: SimpleVisitor): Unit = {
      test = v.field("test", test)
      flag = v.field("flag", flag)
    }
  }

  class Numbers extends SimpleSerializable {
    var int: Int = 0
    var long: Long = 0
    var float: Float = 0
    var double: Double = 0

    def visit(v: SimpleVisitor): Unit = {
      int = v.field("int", int)
      long = v.field("long", long)
      float = v.field("float", float)
      double = v.field("double", double)
    }
  }

  class ListOfTest extends SimpleSerializable {
    var buffer = new ArrayBuffer[Test]()

    def visit(v: SimpleVisitor): Unit = {
      buffer = v.field("buffer", buffer, new Test)
    }
  }

  "SMap.write" should "copy data to simple struct" in {
    val s = SMap("foo" -> SInt(3), "bar" -> SString("Hello"))
    val t = new Test()
    s.write(t)
    assert(t.foo === 3)
    assert(t.bar === "Hello")
  }

  it should "copy data to nested struct" in {
    val inner = SMap("foo" -> SInt(3), "bar" -> SString("Hello"))
    val outer = SMap("test" -> inner, "flag" -> SBool(true))
    val n = new Nested()
    outer.write(n)
    assert(n.test.foo === 3)
    assert(n.test.bar === "Hello")
    assert(n.flag === true)
  }

  it should "cast integer values to int/long/float/double" in {
    val s = SMap(
      "int" -> SInt(1),
      "long" -> SInt(2),
      "float" -> SInt(3),
      "double" -> SInt(4),
    )

    val n = new Numbers()
    s.write(n)
    assert(n.int === 1)
    assert(n.long === 2)
    assert(n.float === 3)
    assert(n.double === 4)
  }

  it should "cast float values to int/long/float/double" in {
    val s = SMap(
      "int" -> SFloat(1),
      "long" -> SFloat(2),
      "float" -> SFloat(3),
      "double" -> SFloat(4),
    )

    val n = new Numbers()
    s.write(n)
    assert(n.int === 1)
    assert(n.long === 2)
    assert(n.float === 3)
    assert(n.double === 4)
  }

  it should "read arrays of stuff" in {
    val s = SMap(
      "buffer" -> SArray(
        SMap("foo" -> SInt(12), "bar" -> SString("Hello")),
        SMap("foo" -> SInt(34), "bar" -> SString("world")),
      ))

    val n = new ListOfTest()
    s.write(n)
    assert(n.buffer.length === 2)
    assert(n.buffer(0).foo === 12)
    assert(n.buffer(0).bar === "Hello")
    assert(n.buffer(1).foo === 34)
    assert(n.buffer(1).bar === "world")
  }

  "SMap.read" should "read data from simple struct" in {
    val test = new Test()
    test.foo = 12
    test.bar = "Hello"

    val m = SMap.read(test)
    assert(m("foo") === SInt(12))
    assert(m("bar") === SString("Hello"))
  }

  it should "read data from a nested struct" in {
    val nested = new Nested()
    nested.test.foo = 3
    nested.test.bar = "Hello"
    nested.flag = true

    val m = SMap.read(nested)
    assert(m.find("test.foo") === SInt(3))
    assert(m.find("test.bar") === SString("Hello"))
    assert(m.find("flag") === SBool(true))
  }

  it should "read integer/float values from int/long/float/double" in {
    val n = new Numbers()
    n.int = 1
    n.long = 2
    n.float = 3
    n.double = 4

    val m = SMap.read(n)
    assert(m("int") === SInt(1))
    assert(m("long") === SInt(2))
    assert(m("float") === SFloat(3))
    assert(m("double") === SFloat(4))
  }

  it should "write arrays of stuff" in {
    val l = new ListOfTest()
    l.buffer += {
      val t = new Test()
      t.foo = 12
      t.bar = "Hello"
      t
    }
    l.buffer += {
      val t = new Test()
      t.foo = 34
      t.bar = "world"
      t
    }

    val m = SMap.read(l)
    val buf = m("buffer").asInstanceOf[SArray]
    assert(buf.length === 2)
    assert(buf(0).asInstanceOf[SMap]("foo") === SInt(12))
    assert(buf(0).asInstanceOf[SMap]("bar") === SString("Hello"))
    assert(buf(1).asInstanceOf[SMap]("foo") === SInt(34))
    assert(buf(1).asInstanceOf[SMap]("bar") === SString("world"))
  }

}
