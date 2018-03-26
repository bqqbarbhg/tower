package io.property

object StringProp {
  type Type = String
}

abstract class StringProp(name: String) extends Property(name) {
  override def getGeneric(inst: PropertyContainer): Any = get(inst)
  override def setGeneric(inst: PropertyContainer, value: Any): Unit = set(inst, value.asInstanceOf[String])

  def get(inst: PropertyContainer): String
  def set(inst: PropertyContainer, value: String): Unit
}

