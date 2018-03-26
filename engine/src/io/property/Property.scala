package io.property

abstract class Property(val name: String) {
  def getGeneric(inst: PropertyContainer): Any
  def setGeneric(inst: PropertyContainer, value: Any): Unit
}
