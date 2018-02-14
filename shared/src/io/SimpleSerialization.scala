package io

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
  }

  case class SArray(v: Seq[SValue]) extends SValue {
    def apply(index: Int): SValue = v.apply(index)
    def length: Int = v.length
  }
}
