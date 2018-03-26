package io.property

abstract class ProductProp(name: String, val propertySet: PropertySet) extends Property(name) {
  def makeProductInstance: PropertyContainer

  def genericToProduct(generic: Any): PropertyContainer
  def productToGeneric(product: PropertyContainer): Any

  def getProductInstance(inst: PropertyContainer): PropertyContainer = genericToProduct(getGeneric(inst))
  def setProductInstance(inst: PropertyContainer, productInst: PropertyContainer): Unit = setGeneric(inst, productToGeneric(productInst))
}
