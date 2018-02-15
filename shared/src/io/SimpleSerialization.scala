package io

import java.lang.reflect.Method

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.collection.mutable.ArrayBuffer

class SimpleSerializeException(message: String) extends RuntimeException(message)

object SimpleSerialization {
  sealed abstract class SValue {
    def kind: String
  }
  case class SString(v: String) extends SValue {
    def kind: String = "string"
  }
  case class SInt(v: Long) extends SValue {
    def kind: String = "integer"
  }
  case class SFloat(v: Double) extends SValue {
    def kind: String = "floating point"
  }
  case class SBool(v: Boolean) extends SValue {
    def kind: String = "boolean"
  }
  case object SNotDefined extends SValue {
    def kind: String = "not defined"
  }

  case class SMap(v: Map[String, SValue]) extends SValue {
    def kind: String = "map"
    def apply(key: String): SValue = {
      val parts = key.split('.')
      parts.foldLeft[SValue](this)((v, p) => v.asInstanceOf[SMap].v(p))
    }

    def write(target: SimpleSerializable): Unit = {
      target.visit(new SMapWrite(this, StrictPartialErrorHandler))
    }

    def writeAll(target: SimpleSerializable): Unit = {
      target.visit(new SMapWrite(this, StrictErrorHandler))
    }
  }

  case class SArray(v: Seq[SValue]) extends SValue {
    def kind: String = "array"
    def apply(index: Int): SValue = v.apply(index)
    def length: Int = v.length
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

  class SMapWrite(m: SMap, eh: ErrorHandler) extends SimpleVisitor {
    private def get(key: String): SValue = m.v.getOrElse(key, SNotDefined)

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

  class SMapRead extends SimpleReadVisitor {
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
      fields(name) = SArray(maps.toSeq)
    }
  }
}
