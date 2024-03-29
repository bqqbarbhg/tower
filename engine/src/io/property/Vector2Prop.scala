package io.property

import java.nio.ByteBuffer

import core._
import io.property.Vector2Prop.ProductInst
import io.serialization.BinarySerializable

object Vector2Prop {
  type Type = Vector2

  class ProductInst extends PropertyContainer {
    override def propertySet: PropertySet = productProps
    var x: DoubleProp.Type = 0.0
    var y: DoubleProp.Type = 0.0
  }

  val productProps = new PropertySet("Vector2", Seq(
    new DoubleProp("x") {
      override def get(inst: PropertyContainer): Double = inst.asInstanceOf[ProductInst].x
      override def set(inst: PropertyContainer, value: Double): Unit = inst.asInstanceOf[ProductInst].x = value
    },
    new DoubleProp("y") {
      override def get(inst: PropertyContainer): Double = inst.asInstanceOf[ProductInst].y
      override def set(inst: PropertyContainer, value: Double): Unit = inst.asInstanceOf[ProductInst].y = value
    },
  ))
}

abstract class Vector2Prop(name: String) extends ProductProp(name, Vector2Prop.productProps) with BinarySerializable {
  override def makeProductInstance: PropertyContainer = new ProductInst()

  override def genericToProduct(generic: Any): PropertyContainer = {
    val v = generic.asInstanceOf[Vector2]
    val pInst = new ProductInst()
    pInst.x = v.x
    pInst.y = v.y
    pInst
  }
  override def productToGeneric(product: PropertyContainer): Any = {
    val pInst = product.asInstanceOf[ProductInst]
    Vector2(pInst.x, pInst.y)
  }

  override def getGeneric(inst: PropertyContainer): Any = get(inst)
  override def setGeneric(inst: PropertyContainer, value: Any): Unit = set(inst, value.asInstanceOf[Vector2])

  def get(inst: PropertyContainer): Vector2
  def set(inst: PropertyContainer, value: Vector2): Unit

  override def writeToBinary(inst: PropertyContainer, buf: ByteBuffer): Unit = {
    val v = get(inst)
    buf.putDouble(v.x)
    buf.putDouble(v.y)
  }
  override def readFromBinary(inst: PropertyContainer, buf: ByteBuffer): Unit = {
    val x = buf.getDouble()
    val y = buf.getDouble()
    set(inst, Vector2(x, y))
  }
  override def sizeInBytes: Int = 16
}

