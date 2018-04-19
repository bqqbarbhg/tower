package io.property

import core._

import scala.reflect.ClassTag

object PropertyProp {
  type Type[A] = A
}

abstract class PropertyProp[A <: PropertyContainer : ClassTag](name: String, propertySet: PropertySet, ctor: () => A) extends ProductProp(name, propertySet) {
  override def getGeneric(inst: PropertyContainer): Any = get(inst)
  override def setGeneric(inst: PropertyContainer, value: Any): Unit = set(inst, value.asInstanceOf[A])
  override def setGenericWithConversion(inst: PropertyContainer, value: Any): Unit = value match {
    case v: A => set(inst, v.asInstanceOf[A])
  }

  def get(inst: PropertyContainer): A
  def set(inst: PropertyContainer, value: A): Unit

  override def makeProductInstance: PropertyContainer = ctor()
  override def genericToProduct(generic: Any): PropertyContainer = generic.asInstanceOf[A]
  override def productToGeneric(product: PropertyContainer): Any = product.asInstanceOf[A]
}

