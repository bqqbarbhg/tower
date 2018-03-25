package io.property

object DoubleProp {
  type Type = Double
}

abstract class DoubleProp(name: String) extends Property(name) {
  override def getGeneric(inst: AnyRef): Any = get(inst)
  override def setGeneric(inst: AnyRef, value: Any): Unit = set(inst, value.asInstanceOf[Double])

  def get(inst: AnyRef): Double
  def set(inst: AnyRef, value: Double): Unit
}

