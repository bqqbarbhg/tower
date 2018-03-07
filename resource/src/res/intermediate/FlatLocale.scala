package res.intermediate

import io.{SimpleSerializable, SimpleVisitor}

import scala.collection.mutable
import io.SimpleSerialization._
import res.intermediate.FlatLocale._

object FlatLocale {

  class Info extends SimpleSerializable {

    /** Code of the language, eg. "en", "fi", "de" */
    var code = ""

    /** Name of the language in the language itself */
    var language = ""

    override def visit(v: SimpleVisitor): Unit = {
      code = v.field("code", code)
      language = v.field("language", language)
    }
  }

  sealed abstract class Part

  /** Reference to `FlatLocale.expressionStringPool` */
  case class Literal(index: Int) extends Part
  /** Reference to `Expression.argNames` */
  case class Argument(index: Int) extends Part

  /**
    * Expression-style locale key, such as "Hello, {name}!"
    */
  case class Expression(parts: Vector[Part], argNames: Vector[String])
}

class FlatLocale extends Resource {
  var info: Info = new Info

  var expressionStringPool = mutable.ArrayBuffer[String]()
  var simpleKeys = mutable.HashMap[String, String]()
  var expressionKeys = mutable.HashMap[String, Expression]()

  override def unload(): Unit = { }
}

