package io.property

object BoolProp {
  type Type = Boolean
}

abstract class BoolProp(name: String) extends Property(name) {
  override def getGeneric(inst: AnyRef): Any = get(inst)
  override def setGeneric(inst: AnyRef, value: Any): Unit = set(inst, value.asInstanceOf[Boolean])

  def get(inst: AnyRef): Boolean
  def set(inst: AnyRef, value: Boolean): Unit
}

