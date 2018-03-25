package io.property

abstract class Property(val name: String) {
  def getGeneric(inst: AnyRef): Any
  def setGeneric(inst: AnyRef, value: Any): Unit
}
