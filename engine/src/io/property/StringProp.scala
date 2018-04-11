package io.property

import core._

object StringProp {
  type Type = String
}

abstract class StringProp(name: String) extends Property(name) {
  override def getGeneric(inst: PropertyContainer): Any = get(inst)
  override def setGeneric(inst: PropertyContainer, value: Any): Unit = set(inst, value.asInstanceOf[String])
  override def setGenericWithConversion(inst: PropertyContainer, value: Any): Unit = value match {
    case v: Identifier => set(inst, v.toString)
    case v: String => set(inst, v)
  }

  def get(inst: PropertyContainer): String
  def set(inst: PropertyContainer, value: String): Unit
}

