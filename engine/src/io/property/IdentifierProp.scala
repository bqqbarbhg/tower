package io.property

import core._

object IdentifierProp {
  type Type = Identifier
}

abstract class IdentifierProp(name: String) extends Property(name) {
  override def getGeneric(inst: PropertyContainer): Any = get(inst)
  override def setGeneric(inst: PropertyContainer, value: Any): Unit = set(inst, value.asInstanceOf[Identifier])
  override def setGenericWithConversion(inst: PropertyContainer, value: Any): Unit = value match {
    case v: Identifier => set(inst, v)
    case v: String => set(inst, Identifier(v))
  }

  def get(inst: PropertyContainer): Identifier
  def set(inst: PropertyContainer, value: Identifier): Unit
}

