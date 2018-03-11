package util

import scala.collection.immutable.VectorBuilder
import scala.collection.mutable

object ArgumentParser {

  /**
    * Represents parsed argument values.
    *
    * Example: hello.txt --standalone -i --num=10 -o thing.out other.txt -L=lib -L=other
    * called with `parse(args, implicitArgumentFlags=Vector("o"), multiArgumentFlags=Vector("L"))`
    *
    * @param positional Positional non-flag arguments, in example: [hello.txt, other.txt]
    * @param keywords Flags with values, in example: [num -> 10, o -> thing.out]
    * @param multiKeywords Flags with multiple values, in example: [L -> [lib, other]]
    * @param flags Set of enabled flags, in example: [standalone, i]
    */
  case class Args(
    positional: Vector[String],
    keywords: Map[String, String],
    multiKeywords: Map[String, Vector[String]],
    flags: Set[String]) {

    /** Returns whether a flag is set */
    def flag(flag: String): Boolean = flags.contains(flag)

  }

  /**
    * Parse arguments supplied to a command-line program.
    *
    * @param args Arguments passed to the program, found in `args` from Scala's `App`.
    * @param implicitArgumentFlags Determines flags that should be treated as taking an value.
    *                              For example if set to `"o"` then "-o foo.txt" would be
    *                              parsed as "-o=foo.txt"
    * @param multiArgumentFlags Determines flags that can have multiple values at the same
    *                           time. For example if `"L"` then "-L=first -L=second" will
    *                           result in `L = [first, second]`. Otherwise only the last
    *                           argument is taken.
    * @param aliases Aliases for flags. For example if `"F" -> "flag"` then "-F" will be
    *                parsed as "-flag"
    */
  def parse(args: Iterable[String],
            implicitArgumentFlags: Iterable[String] = Vector(),
            multiArgumentFlags: Iterable[String] = Vector(),
            aliases: Map[String, String] = Map[String, String]()): Args = {
    val implicitArgs = implicitArgumentFlags.toSet
    val pos = new VectorBuilder[String]
    val kw = new mutable.HashMap[String, String]()
    val multiKw = new mutable.HashMap[String, VectorBuilder[String]]()
    val flags = new mutable.HashSet[String]()

    // Create empty containers for all the multi-keywords
    for (flag <- multiArgumentFlags) {
      multiKw(flag) = new VectorBuilder[String]
    }

    def setKw(key: String, value: String): Unit = {
      val maybeMulti = multiKw.get(key)
      maybeMulti match {
        case Some(multi) => multi += value
        case None => kw(key) = value
      }
    }

    var implicitKeywordActivated = ""
    for (arg <- args.filter(_.nonEmpty)) {
      // Implicit keywords take priority
      if (implicitKeywordActivated.nonEmpty) {
        setKw(implicitKeywordActivated, arg)
        implicitKeywordActivated = ""
      } else if (arg.head == '-') {
        val tail = arg.dropWhile(_ == '-')
        val eqIx = tail.indexOf('=')
        val rawFlag = if (eqIx >= 0) tail.take(eqIx) else tail
        val flag = aliases.get(rawFlag).getOrElse(rawFlag)

        if (eqIx >= 0) {
          // -foo=thing
          val value = tail.drop(eqIx + 1)
          setKw(flag, value)
        } else if (implicitArgs.contains(flag)) {
          // -o thing
          implicitKeywordActivated = flag
        } else {
          // --flag
          flags += flag
        }
      } else {
        pos += arg
      }
    }

    val multi = multiKw.mapValues(_.result).toMap
    Args(pos.result, kw.toMap, multi, flags.toSet)
  }
}
