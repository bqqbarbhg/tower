package io

import java.lang.reflect.Method

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer

class SimpleSerializeException(message: String) extends RuntimeException(message)

object SimpleSerialization {

  sealed abstract class SValue {
    /** Textual description of the type of data */
    def kind: String
  }

  case class  SString(v: String)  extends SValue { def kind: String = "string"         }
  case class  SInt   (v: Long)    extends SValue { def kind: String = "integer"        }
  case class  SFloat (v: Double)  extends SValue { def kind: String = "floating point" }
  case class  SBool  (v: Boolean) extends SValue { def kind: String = "boolean"        }
  case object SNotDefined         extends SValue { def kind: String = "not defined"    }

  object SMap {

    /** Read a serializable object into a `SMap` */
    def read(target: SimpleSerializable): SMap = {
      val state = new SMapRead()
      target.visit(state)
      state.map
    }

    /** Construct an `SMap` from pairs maintaining the order */
    def apply(pairs: (String, SValue)*): SMap = new SMap(pairs)
  }

  case class SMap(data: Map[String, SValue], order: Vector[String]) extends SValue {
    def kind: String = "map"

    /** Construct an `SMap` from pairs maintaining the order */
    def this(pairs: Iterable[(String, SValue)]) = this(pairs.toMap, pairs.map(_._1).toVector)

    /** Returns all the key/value pairs in order */
    def pairs: Vector[(String, SValue)] = order.map(key => (key, data(key)))

    /** Returns a property with the name `key` */
    def apply(key: String): SValue = data.getOrElse(key, SNotDefined)

    /** Returns a nested property with dot seprated keys */
    def find(keys: String): SValue = {
      val parts = keys.split('.')
      var node = this
      for (part <- parts.dropRight(1)) {
        node(part) match {
          case child: SMap => node = child
          case _ => return SNotDefined
        }
      }
      node(parts.last)
    }

    /** Write the contents of this map to a serializable object, ignores missing keys */
    def write(target: SimpleSerializable, errorHandler: ErrorHandler = StrictPartialErrorHandler): Unit = {
      target.visit(new SMapWrite(this, errorHandler))
    }

    /** Write the contents of this map to a serializable object, throws an error on missing keys */
    def writeAll(target: SimpleSerializable, errorHandler: ErrorHandler = StrictErrorHandler): Unit = {
      target.visit(new SMapWrite(this, errorHandler))
    }
  }

  object SArray {
    def apply(data: SValue*): SArray = new SArray(data)
  }

  case class SArray(data: Vector[SValue]) extends SValue {
    def this(data: Iterable[SValue]) = this(data.toVector)

    def kind: String = "array"
    def apply(index: Int): SValue = data.lift(index).getOrElse(SNotDefined)
    def length: Int = data.length
  }

  trait ErrorHandler {
    def notFound(name: String, expected: String): Unit
    def wrongType(name: String, expected: String, got: String): Unit
  }

  object StrictErrorHandler extends ErrorHandler {
    def notFound(name: String, expected: String): Unit = {
      throw new SimpleSerializeException(s"Key '$name' not found, expected $expected")
    }
    def wrongType(name: String, expected: String, got: String): Unit = {
      throw new SimpleSerializeException(s"Key '$name' was defined as $got, expected $expected")
    }
  }

  object StrictPartialErrorHandler extends ErrorHandler {
    def notFound(name: String, expected: String): Unit = { /* Ignore missing */ }
    def wrongType(name: String, expected: String, got: String): Unit = {
      throw new SimpleSerializeException(s"Key '$name' was defined as $got, expected $expected")
    }
  }

  private class SMapWrite(m: SMap, eh: ErrorHandler) extends SimpleVisitor {
    private def get(key: String): SValue = m(key)

    def field(v: SimpleVisitor, name: String, value: Int): Int = {
      get(name) match {
        case SInt(i) => i.toInt
        case SFloat(i) => i.toInt
        case SNotDefined => eh.notFound(name, "int"); value
        case other => eh.wrongType(name, "int", other.kind); value
      }
    }

    def field(v: SimpleVisitor, name: String, value: Long): Long = {
      get(name) match {
        case SInt(i) => i
        case SFloat(i) => i.toLong
        case SNotDefined => eh.notFound(name, "long"); value
        case other => eh.wrongType(name, "long", other.kind); value
      }
    }

    def field(v: SimpleVisitor, name: String, value: Float): Float = {
      get(name) match {
        case SInt(i) => i.toFloat
        case SFloat(i) => i.toFloat
        case SNotDefined => eh.notFound(name, "float"); value
        case other => eh.wrongType(name, "float", other.kind); value
      }
    }

    def field(v: SimpleVisitor, name: String, value: Double): Double = {
      get(name) match {
        case SInt(i) => i.toDouble
        case SFloat(i) => i
        case SNotDefined => eh.notFound(name, "double"); value
        case other => eh.wrongType(name, "double", other.kind); value
      }
    }

    def field(v: SimpleVisitor, name: String, value: String): String = {
      get(name) match {
        case SString(s) => s
        case SNotDefined => eh.notFound(name, "string"); value
        case other => eh.wrongType(name, "string", other.kind); value
      }
    }

    def field(v: SimpleVisitor, name: String, value: Boolean): Boolean = {
      get(name) match {
        case SBool(b) => b
        case SNotDefined => eh.notFound(name, "boolean"); value
        case other => eh.wrongType(name, "boolean", other.kind); value
      }
    }

    def field[T <: SimpleSerializable : ClassTag](v: SimpleVisitor, name: String, value: T): T = {
      get(name) match {
        case child: SMap =>
          value.visit(new SMapWrite(child, eh))
          value
        case SNotDefined => eh.notFound(name, "map"); value
        case other => eh.wrongType(name, "map", other.kind); value
      }
    }

    def field[T <: SimpleSerializable : ClassTag](v: SimpleVisitor, name: String, value: ArrayBuffer[T], ctor: => T): ArrayBuffer[T] = {
      get(name) match {
        case SArray(arr) =>
          for (v <- arr) {
            v match {
              case child: SMap =>
                val inst = ctor
                inst.visit(new SMapWrite(child, eh))
                value += inst
              case other => eh.wrongType(name, "map inside array", other.kind); value
            }
          }
          value
        case SNotDefined => eh.notFound(name, "array"); value
        case other => eh.wrongType(name, "array", other.kind); value
      }
    }
  }

  private class SMapRead extends SimpleReadVisitor {
    private val fields = mutable.HashMap[String, SValue]()

    def map: SMap = new SMap(fields.toMap)

    def readField(v: SimpleVisitor, name: String, value: Int): Unit = { fields(name) = SInt(value) }
    def readField(v: SimpleVisitor, name: String, value: Long): Unit = { fields(name) = SInt(value) }
    def readField(v: SimpleVisitor, name: String, value: Float): Unit = { fields(name) = SFloat(value) }
    def readField(v: SimpleVisitor, name: String, value: Double): Unit = { fields(name) = SFloat(value) }
    def readField(v: SimpleVisitor, name: String, value: String): Unit = { fields(name) = SString(value) }
    def readField(v: SimpleVisitor, name: String, value: Boolean): Unit = { fields(name) = SBool(value) }
    def readField[T <: SimpleSerializable : ClassTag](v: SimpleVisitor, name: String, value: T): Unit = {
      val child = new SMapRead()
      value.visit(child)
      fields(name) = child.map
    }
    def readField[T <: SimpleSerializable : ClassTag](v: SimpleVisitor, name: String, value: ArrayBuffer[T], ctor: => T): Unit = {
      val maps = for (v <- value) yield {
        val child = new SMapRead()
        v.visit(child)
        child.map
      }
      fields(name) = SArray(maps.toVector)
    }
  }
}
