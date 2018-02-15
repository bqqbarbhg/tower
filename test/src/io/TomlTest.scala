package io

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics
import io.SimpleSerialization._

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class TomlTest extends FlatSpec with Matchers {

  "Toml" should "parse simple toplevel table" in {
    val fixture =
      """
        |# Comment
        |str = "Hello world!" # Comment
        |int = 10
        |float = 1.0
        |yes = true
        |no = false
      """.stripMargin

    val v = Toml.parse(fixture, "test")
    assert(v("str") === SString("Hello world!"))
    assert(v("int") === SInt(10))
    assert(v("float") === SFloat(1.0))
    assert(v("yes") === SBool(true))
    assert(v("no") === SBool(false))
  }

  it should "allow underscores in numbers" in {
    val fixture =
      """
        |a = 1234
        |b = 12_34
      """.stripMargin

    val v = Toml.parse(fixture, "test")
    assert(v("a") === SInt(1234))
    assert(v("b") === SInt(1234))
  }

  it should "support nested keys" in {
    val fixture =
      """
        |map.foo.thing = 10
      """.stripMargin

    val v = Toml.parse(fixture, "test")
    assert(v("map.foo.thing") === SInt(10))
  }

  it should "support nested keys in context" in {
    val fixture =
      """
        |[map.foo] # Comment
        |thing = 10
        |[[some.list]]
        |thing = 15
      """.stripMargin

    val v = Toml.parse(fixture, "test")
    val arr = v("some.list").asInstanceOf[SArray]
    assert(v("map.foo.thing") === SInt(10))
    assert(arr(0).asInstanceOf[SMap]("thing") === SInt(15))
    assert(arr.length === 1)
  }

  it should "support arrays of tables" in {
    val fixture =
      """
        |[[things]]
        |value = 5
        |[[things]]
        |value = 10
        |other.value = 15
      """.stripMargin

    val v = Toml.parse(fixture, "test")
    val arr = v("things").asInstanceOf[SArray]
    assert(arr(0).asInstanceOf[SMap]("value") === SInt(5))
    assert(arr(1).asInstanceOf[SMap]("value") === SInt(10))
    assert(arr(1).asInstanceOf[SMap]("other.value") === SInt(15))
    assert(arr.length === 2)
  }

  class Test extends SimpleSerializable {
    var foo = 0
    var bar = ""

    def visit(v: SimpleVisitor): Unit = {
      foo = v.field(v, "foo", foo)
      bar = v.field(v, "bar", bar)
    }
  }

  class Nested extends SimpleSerializable {
    var test = new Test()
    var flag = false

    def visit(v: SimpleVisitor): Unit = {
      test = v.field(v, "test", test)
      flag = v.field(v, "flag", flag)
    }
  }

  class Numbers extends SimpleSerializable {
    var int: Int = 0
    var long: Long = 0
    var float: Float = 0
    var double: Double = 0

    def visit(v: SimpleVisitor): Unit = {
      int = v.field(v, "int", int)
      long = v.field(v, "long", long)
      float = v.field(v, "float", float)
      double = v.field(v, "double", double)
    }
  }

  class ListOfTest extends SimpleSerializable {
    var buffer = new ArrayBuffer[Test]()

    def visit(v: SimpleVisitor): Unit = {
      buffer = v.field(v, "buffer", buffer, new Test)
    }
  }

  "SimpleSerialization write" should "copy data to simple struct" in {
    val s = SMap(Map("foo" -> SInt(3), "bar" -> SString("Hello")))
    val t = new Test()
    s.write(t)
    assert(t.foo === 3)
    assert(t.bar === "Hello")
  }

  it should "copy data to nested struct" in {
    val inner = SMap(Map("foo" -> SInt(3), "bar" -> SString("Hello")))
    val outer = SMap(Map("test" -> inner, "flag" -> SBool(true)))
    val n = new Nested()
    outer.write(n)
    assert(n.test.foo === 3)
    assert(n.test.bar === "Hello")
    assert(n.flag === true)
  }

  it should "cast integer values to int/long/float/double" in {
    val s = SMap(Map(
      "int" -> SInt(1),
      "long" -> SInt(2),
      "float" -> SInt(3),
      "double" -> SInt(4),
    ))

    val n = new Numbers()
    s.write(n)
    assert(n.int === 1)
    assert(n.long === 2)
    assert(n.float === 3)
    assert(n.double === 4)
  }

  it should "read arrays of stuff" in {
    val s = SMap(Map(
      "buffer" -> SArray(Vector(
      SMap(Map("foo" -> SInt(12), "bar" -> SString("Hello"))),
      SMap(Map("foo" -> SInt(34), "bar" -> SString("world"))),
    ))))

    val n = new ListOfTest()
    s.write(n)
    assert(n.buffer.length === 2)
    assert(n.buffer(0).foo === 12)
    assert(n.buffer(0).bar === "Hello")
    assert(n.buffer(1).foo === 34)
    assert(n.buffer(1).bar === "world")
  }

  "SimpleSerialization read" should "read data from simple struct" in {
    val test = new Test()
    test.foo = 12
    test.bar = "Hello"

    val m = SMap.read(test)
    assert(m("foo") === SInt(12))
    assert(m("bar") === SString("Hello"))
  }

}
