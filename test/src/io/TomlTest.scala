package io

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics

import io.SimpleSerialization._

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

}
