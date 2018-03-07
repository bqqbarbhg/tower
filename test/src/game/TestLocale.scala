package game

import locale.{LocaleGetter, Locale}

object TestLocale {

  val Test = new LocaleGetter("Test") {
    private val key_helloWorld: Int = addSimple("helloWorld")
    def helloWorld: String = Locale.getSimple(key_helloWorld)

    private val key_welcome: Int = addExpression("welcome", "name")
    def welcome(name: String): String = Locale.getExpression(key_welcome, Array(name))

    private val key_escaped: Int = addSimple("escaped")
    def escaped: String = Locale.getSimple(key_escaped)
  }

}

