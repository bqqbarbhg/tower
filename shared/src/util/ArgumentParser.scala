package util

import scala.collection.immutable.VectorBuilder
import scala.collection.mutable

object ArgumentParser {

  /**
    * Represents parsed argument values.
    *
    * Example: hello.txt --standalone -i --num=10 -o thing.out other.txt
    * called with `parse(args, implicitArgumentFlags=Vector("-o"))`
    *
    * @param positional Positional non-flag arguments, in example: [hello.txt, other.txt]
    * @param keywords Flags with values, in example: [num -> 10, o -> thing.out]
    * @param flags Set of enabled flags, in example: [standalone, i]
    */
  case class Args(positional: Vector[String], keywords: Map[String, String], flags: Set[String]) {

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
    */
  def parse(args: Iterable[String], implicitArgumentFlags: Iterable[String] = Vector()): Args = {
    val implicitArgs = implicitArgumentFlags.toSet
    val pos = new VectorBuilder[String]
    val kw = new mutable.HashMap[String, String]()
    val flag = new mutable.HashSet[String]()

    var implicitKeywordActivated = ""
    for (arg <- args.filter(_.nonEmpty)) {
      // Implicit keywords take priority
      if (implicitKeywordActivated.nonEmpty) {
        kw(implicitKeywordActivated) = arg
        implicitKeywordActivated = ""
      } else if (arg.head == '-') {
        val tail = arg.dropWhile(_ == '-')
        val eqIx = tail.indexOf('=')
        if (eqIx >= 0) {
          // -foo=thing
          val key = tail.take(eqIx)
          val value = tail.drop(eqIx + 1)
          kw(key) = value
        } else if (implicitArgs.contains(tail)) {
          // -o thing
          implicitKeywordActivated = tail
        } else {
          // --flag
          flag += tail
        }
      } else {
        pos += arg
      }
    }

    Args(pos.result, kw.toMap, flag.toSet)
  }
}
