package io

import java.lang.reflect.Method
import scala.reflect.ClassTag
import scala.collection.mutable

object SimpleSerialization {
  sealed abstract class SValue
  case class SString(v: String) extends SValue
  case class SInt(v: Long) extends SValue
  case class SFloat(v: Double) extends SValue
  case class SBool(v: Boolean) extends SValue

  case class SMap(v: Map[String, SValue]) extends SValue {
    def apply(key: String): SValue = {
      val parts = key.split('.')
      parts.foldLeft[SValue](this)((v, p) => v.asInstanceOf[SMap].v(p))
    }

    def write(ref: AnyRef, cls: Class[_]): Unit = {
      val fields = getFields(cls)

      for ((k, v) <- v) {
        v match {
          case SString(s) => fields.set(k).invoke(ref, s)
          case SInt(i) =>
            val m = fields.set(k)
            val cls = m.getParameterTypes()(0)
            if (cls == java.lang.Integer.TYPE) m.invoke(ref, Int.box(i.toInt))
            else if (cls == java.lang.Long.TYPE) m.invoke(ref, Long.box(i))
            else if (cls == java.lang.Double.TYPE) m.invoke(ref, Double.box(i.toDouble))
            else if (cls == java.lang.Float.TYPE) m.invoke(ref, Float.box(i.toFloat))
            else throw new RuntimeException(s"Incompatible class for integer: ${cls.getName}")
          case SFloat(f) => fields.set(k).invoke(ref, Double.box(f))
          case SBool(b) => fields.set(k).invoke(ref, Boolean.box(b))
          case map: SMap =>
            val m = fields.get(k)
            val obj = m.invoke(ref)
            map.write(obj, m.getReturnType)
          case arr: SArray => assert(false, "TODO")
        }
      }
    }

    /**
      * Assign the contents of this map to a class. Recursively copies values
      * to fields from inner maps.
      */
    def write[T <: AnyRef : ClassTag](t: T): Unit = write(t, t.getClass)
  }

  case class SArray(v: Seq[SValue]) extends SValue {
    def apply(index: Int): SValue = v.apply(index)
    def length: Int = v.length
  }

  case class FieldCache(get: Map[String, Method], set: Map[String, Method])

  private val typeCache = new mutable.HashMap[Class[_], FieldCache]()
  def getFields(c: Class[_]): FieldCache = typeCache.synchronized {
    typeCache.getOrElseUpdate(c, {
      val get = c.getMethods.filterNot(_.getName.endsWith("_$eq")).map(m => (m.getName, m)).toMap
      val set = c.getMethods.filter(_.getName.endsWith("_$eq")).map(m => (m.getName.dropRight(4), m)).toMap
      FieldCache(get, set)
    })
  }
}
