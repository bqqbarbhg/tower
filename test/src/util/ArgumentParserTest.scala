package util

import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalactic.TolerantNumerics
import io.SimpleSerialization._

import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class ArgumentParserTest extends FlatSpec with Matchers {

  "ArgumentParser.parse" should "parse positional arguments" in {
    val args = "hello world yes 10".split(' ')
    val arg = ArgumentParser.parse(args)

    assert(arg.positional.length === 4)
    assert(arg.positional(0) === "hello")
    assert(arg.positional(1) === "world")
    assert(arg.positional(2) === "yes")
    assert(arg.positional(3) === "10")
    assert(arg.keywords.isEmpty)
    assert(arg.multiKeywords.isEmpty)
    assert(arg.flags.isEmpty)
  }

  it should "parse keyword arguments" in {
    val args = "--key=val -o=file".split(' ')
    val arg = ArgumentParser.parse(args)

    assert(arg.positional.isEmpty)
    assert(arg.keywords.size === 2)
    assert(arg.keywords("key") === "val")
    assert(arg.keywords("o") === "file")
    assert(arg.multiKeywords.isEmpty)
    assert(arg.flags.isEmpty)
  }

  it should "parse flags" in {
    val args = "-f --force".split(' ')
    val arg = ArgumentParser.parse(args)

    assert(arg.positional.isEmpty)
    assert(arg.keywords.isEmpty)
    assert(arg.multiKeywords.isEmpty)
    assert(arg.flags.size === 2)
    assert(arg.flags.contains("f"))
    assert(arg.flags.contains("force"))
  }

  it should "parse a mixture of positional, keywords and flags" in {
    val args = "pos -flag --key=val".split(' ')
    val arg = ArgumentParser.parse(args)

    assert(arg.positional.size === 1)
    assert(arg.positional(0) === "pos")
    assert(arg.keywords.size === 1)
    assert(arg.keywords("key") === "val")
    assert(arg.multiKeywords.isEmpty)
    assert(arg.flags.size === 1)
    assert(arg.flags.contains("flag"))
  }

  it should "ignore only leading extra dashes '-'" in {
    val args = "----------------------------------extra--dashes---".split(' ')
    val arg = ArgumentParser.parse(args)

    assert(arg.positional.isEmpty)
    assert(arg.keywords.isEmpty)
    assert(arg.multiKeywords.isEmpty)
    assert(arg.flags.size === 1)
    assert(arg.flags.contains("extra--dashes---"))
  }

  "implicitArgumentFlags" should "cause flags to be parsed as keywords" in {
    val args = "-o foo --key val".split(' ')
    val arg = ArgumentParser.parse(args, implicitArgumentFlags = Vector("o", "key"))

    assert(arg.positional.isEmpty)
    assert(arg.keywords.size === 2)
    assert(arg.keywords("o") === "foo")
    assert(arg.keywords("key") === "val")
    assert(arg.multiKeywords.isEmpty)
    assert(arg.flags.isEmpty)
  }

  it should "not interfere with normal keyword syntax" in {
    val args = "-o=foo --key=val".split(' ')
    val arg = ArgumentParser.parse(args, implicitArgumentFlags = Vector("o", "key"))

    assert(arg.positional.isEmpty)
    assert(arg.keywords.size === 2)
    assert(arg.keywords("o") === "foo")
    assert(arg.keywords("key") === "val")
    assert(arg.multiKeywords.isEmpty)
    assert(arg.flags.isEmpty)
  }

  it should "fail gracefully without further arguments" in {
    val args = "-o foo --key".split(' ')
    val arg = ArgumentParser.parse(args, implicitArgumentFlags = Vector("o", "key"))

    assert(arg.positional.isEmpty)
    assert(arg.keywords.size === 1)
    assert(arg.keywords("o") === "foo")
    assert(arg.multiKeywords.isEmpty)
    assert(arg.flags.isEmpty)
  }

  "multiArgumentFlags" should "parse multi keyword arguments" in {
    val args = "-L=first -L=second".split(' ')
    val arg = ArgumentParser.parse(args,
      multiArgumentFlags = Vector("L"))

    assert(arg.positional.isEmpty)
    assert(arg.keywords.isEmpty)
    assert(arg.multiKeywords.size === 1)
    assert(arg.multiKeywords("L").size === 2)
    assert(arg.multiKeywords("L")(0) === "first")
    assert(arg.multiKeywords("L")(1) === "second")
    assert(arg.flags.isEmpty)
  }

  it should "work with implicitArgumentFlags" in {
    val args = "-L first -L second".split(' ')
    val arg = ArgumentParser.parse(args,
      implicitArgumentFlags = Vector("L"),
      multiArgumentFlags = Vector("L"))

    assert(arg.positional.isEmpty)
    assert(arg.keywords.isEmpty)
    assert(arg.multiKeywords.size === 1)
    assert(arg.multiKeywords("L").size === 2)
    assert(arg.multiKeywords("L")(0) === "first")
    assert(arg.multiKeywords("L")(1) === "second")
    assert(arg.flags.isEmpty)
  }

  "aliases" should "map argument names with aliases" in {
    val args = "-F".split(' ')
    val arg = ArgumentParser.parse(args, aliases = Map("F" -> "flag"))

    assert(arg.positional.isEmpty)
    assert(arg.keywords.isEmpty)
    assert(arg.multiKeywords.isEmpty)
    assert(arg.flags.size === 1)
    assert(arg.flags.contains("flag"))
  }

  it should "not mess with the original name" in {
    val args = "-flag".split(' ')
    val arg = ArgumentParser.parse(args, aliases = Map("F" -> "flag"))

    assert(arg.positional.isEmpty)
    assert(arg.keywords.isEmpty)
    assert(arg.multiKeywords.isEmpty)
    assert(arg.flags.size === 1)
    assert(arg.flags.contains("flag"))
  }

  it should "not affect positional arguments" in {
    val args = "F".split(' ')
    val arg = ArgumentParser.parse(args, aliases = Map("F" -> "flag"))

    assert(arg.positional.size === 1)
    assert(arg.positional(0) === "F")
    assert(arg.keywords.isEmpty)
    assert(arg.multiKeywords.isEmpty)
    assert(arg.flags.isEmpty)
  }

  it should "work with keyword arguments" in {
    val args = "-F=value".split(' ')
    val arg = ArgumentParser.parse(args, aliases = Map("F" -> "flag"))

    assert(arg.positional.isEmpty)
    assert(arg.keywords.size === 1)
    assert(arg.keywords("flag") === "value")
    assert(arg.multiKeywords.isEmpty)
    assert(arg.flags.isEmpty)
  }

  it should "work with implicit keyword arguments" in {
    val args = "-F value".split(' ')
    val arg = ArgumentParser.parse(args,
      implicitArgumentFlags = Vector("flag"),
      aliases = Map("F" -> "flag"))

    assert(arg.positional.isEmpty)
    assert(arg.keywords.size === 1)
    assert(arg.keywords("flag") === "value")
    assert(arg.multiKeywords.isEmpty)
    assert(arg.flags.isEmpty)
  }

  it should "work with multi-keyword arguments" in {
    val args = "-F=value -F=other -flag=old".split(' ')
    val arg = ArgumentParser.parse(args,
      multiArgumentFlags = Vector("flag"),
      aliases = Map("F" -> "flag"))

    assert(arg.positional.isEmpty)
    assert(arg.keywords.isEmpty)
    assert(arg.multiKeywords.size === 1)
    assert(arg.multiKeywords("flag").size === 3)
    assert(arg.multiKeywords("flag")(0) === "value")
    assert(arg.multiKeywords("flag")(1) === "other")
    assert(arg.multiKeywords("flag")(2) === "old")
    assert(arg.flags.isEmpty)
  }

  it should "work with implicit multi-keyword arguments" in {
    val args = "-F value -F=other -flag=old".split(' ')
    val arg = ArgumentParser.parse(args,
      implicitArgumentFlags = Vector("flag"),
      multiArgumentFlags = Vector("flag"),
      aliases = Map("F" -> "flag"))

    assert(arg.positional.isEmpty)
    assert(arg.keywords.isEmpty)
    assert(arg.multiKeywords.size === 1)
    assert(arg.multiKeywords("flag").size === 3)
    assert(arg.multiKeywords("flag")(0) === "value")
    assert(arg.multiKeywords("flag")(1) === "other")
    assert(arg.multiKeywords("flag")(2) === "old")
    assert(arg.flags.isEmpty)
  }
}
