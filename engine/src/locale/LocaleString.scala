package locale

object LocaleString {

  implicit class LocaleHelper(val sc: StringContext) extends AnyVal {
    def lc(args: Any*): String = {
      val key = if (sc.parts.size == 1) {
        sc.parts.head
      } else {
        val str = sc.parts.iterator
        val exp = args.iterator
        val buf = new StringBuilder(str.next)
        while (str.hasNext) {
          buf.append(exp.next)
          buf.append(str.next)
        }
        buf.result
      }

      Locale.getSimple(key)
    }
  }

}
