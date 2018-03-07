package locale

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LocaleGetter {

  val simpleLocales = mutable.HashMap[String, Int]()
  val expressionLocales = mutable.HashMap[String, Int]()
  val expressionArgNames = ArrayBuffer[Array[String]]()

}

class LocaleGetter(path: String) {

  /**
    * Register a new simple locale key `key`. Returns index of the key that can
    * be used with `Locale.getSimple`.
    */
  protected def addSimple(key: String): Int = {
    val fullKey = path + "." + key
    LocaleGetter.simpleLocales.getOrElseUpdate(fullKey, LocaleGetter.simpleLocales.size)
  }

  /**
    * Register a new expression locale key `key`. Returns index of the key that
    * can be used with `Locale.getExpression`.
    */
  protected def addExpression(key: String, args: String*): Int = {
    val fullKey = path + "." + key
    LocaleGetter.expressionLocales.getOrElseUpdate(fullKey, {
      val index = LocaleGetter.expressionLocales.size
      LocaleGetter.expressionArgNames += args.toArray
      index
    })
  }

}

