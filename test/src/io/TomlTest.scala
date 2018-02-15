package io

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics
import io.SimpleSerialization._

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class TomlTest extends FlatSpec with Matchers {

  "Toml.parse" should "parse simple toplevel table" in {
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

  it should "should handle negative numbers" in {
    val fixture =
      """
        |a = -10
        |b = -10.0
      """.stripMargin

    val v = Toml.parse(fixture, "test")
    assert(v("a") === SInt(-10))
    assert(v("b") === SFloat(-10.0))
  }

  it should "support nested keys" in {
    val fixture =
      """
        |map.foo.thing = 10
      """.stripMargin

    val v = Toml.parse(fixture, "test")
    assert(v.find("map.foo.thing") === SInt(10))
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
    val arr = v.find("some.list").asInstanceOf[SArray]
    assert(v.find("map.foo.thing") === SInt(10))
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
    assert(arr(1).asInstanceOf[SMap].find("other.value") === SInt(15))
    assert(arr.length === 2)
  }

  "Toml.format" should "format simple toplevel table maintaining order" in {
    val fixture = SMap(
      "str" -> SString("Hello world!"),
      "int" -> SInt(10),
      "float" -> SFloat(1.0),
      "yes" -> SBool(true),
      "no" -> SBool(false),
    )

    val s = Toml.format(fixture)
    val lines = s.split("\n")
    assert(lines.length === 5)
    assert(lines(0) === "str = \"Hello world!\"")
    assert(lines(1) === "int = 10")
    assert(lines(2) === "float = 1.0")
    assert(lines(3) === "yes = true")
    assert(lines(4) === "no = false")
  }

  it should "support nested properties" in {
    val fixture = SMap(
      "toplevel" -> SBool(true),
      "nested" -> SMap(
        "thing" -> SInt(10),
        "inner" -> SMap(
          "value" -> SInt(1234))))

    val s = Toml.format(fixture)
    val lines = s.split("\n")
    assert(lines.length === 7)
    assert(lines(0) === "toplevel = true")
    assert(lines(1) === "")
    assert(lines(2) === "[nested]")
    assert(lines(3) === "thing = 10")
    assert(lines(4) === "")
    assert(lines(5) === "[nested.inner]")
    assert(lines(6) === "value = 1234")
  }

  it should "omit unnecessary objects" in {
    val fixture = SMap(
      "toplevel" -> SBool(true),
      "nested" -> SMap(
        "inner" -> SMap(
          "value" -> SInt(1234))))

    val s = Toml.format(fixture)
    val lines = s.split("\n")
    assert(lines.length === 4)
    assert(lines(0) === "toplevel = true")
    assert(lines(1) === "")
    assert(lines(2) === "[nested.inner]")
    assert(lines(3) === "value = 1234")
  }

  it should "support arrays of objects" in {
    val fixture = SMap(
      "array" -> SArray(
        SMap("value" -> SInt(10)),
        SMap("value" -> SInt(20)),
      ))

    val s = Toml.format(fixture)
    val lines = s.split("\n")
    assert(lines.length === 6)
    assert(lines(0) === "")
    assert(lines(1) === "[[array]]")
    assert(lines(2) === "value = 10")
    assert(lines(3) === "")
    assert(lines(4) === "[[array]]")
    assert(lines(5) === "value = 20")
  }

  it should "make nested keys inline inside an array" in {
    val fixture = SMap(
      "array" -> SArray(
        SMap("value" -> SInt(10)),
        SMap("value" -> SInt(20),
          "nested" -> SMap("thing" -> SInt(30)),
      )))

    val s = Toml.format(fixture)
    val lines = s.split("\n")
    assert(lines.length === 7)
    assert(lines(0) === "")
    assert(lines(1) === "[[array]]")
    assert(lines(2) === "value = 10")
    assert(lines(3) === "")
    assert(lines(4) === "[[array]]")
    assert(lines(5) === "value = 20")
    assert(lines(6) === "nested.thing = 30")
  }

  "Toml.parse(Toml.format)" should "roundtrip data with difficult fixture" in {
    val fixture = SMap(
      "thing" -> SBool(true),
      "float" -> SFloat(-3.0),
      "array" -> SArray(
        SMap("value" -> SInt(10), "other" -> SBool(false)),
        SMap("value" -> SInt(20),
          "nested" -> SMap("thing" -> SInt(30)),
        )))

    val formatted = Toml.format(fixture)
    println(formatted)
    val parsed = Toml.parse(formatted, "roundtrip")
    assert(fixture === parsed)
  }

}
