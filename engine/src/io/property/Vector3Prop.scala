package io.property

import java.nio.ByteBuffer

import core._
import io.property.Vector3Prop.ProductInst
import io.serialization.BinarySerializable

object Vector3Prop {
  type Type = Vector3

  class ProductInst extends PropertyContainer {
    override def propertySet: PropertySet = productProps
    var x: DoubleProp.Type = 0.0
    var y: DoubleProp.Type = 0.0
    var z: DoubleProp.Type = 0.0
  }

  val productProps = new PropertySet("Vector3", Seq(
    new DoubleProp("x") {
      override def get(inst: PropertyContainer): Double = inst.asInstanceOf[ProductInst].x
      override def set(inst: PropertyContainer, value: Double): Unit = inst.asInstanceOf[ProductInst].x = value
    },
    new DoubleProp("y") {
      override def get(inst: PropertyContainer): Double = inst.asInstanceOf[ProductInst].y
      override def set(inst: PropertyContainer, value: Double): Unit = inst.asInstanceOf[ProductInst].y = value
    },
    new DoubleProp("z") {
      override def get(inst: PropertyContainer): Double = inst.asInstanceOf[ProductInst].z
      override def set(inst: PropertyContainer, value: Double): Unit = inst.asInstanceOf[ProductInst].z = value
    },
  ))
}

abstract class Vector3Prop(name: String) extends ProductProp(name, Vector3Prop.productProps) with BinarySerializable {
  override def makeProductInstance: PropertyContainer = new ProductInst()

  override def genericToProduct(generic: Any): PropertyContainer = {
    val v = generic.asInstanceOf[Vector3]
    val pInst = new ProductInst()
    pInst.x = v.x
    pInst.y = v.y
    pInst.z = v.z
    pInst
  }
  override def productToGeneric(product: PropertyContainer): Any = {
    val pInst = product.asInstanceOf[ProductInst]
    Vector3(pInst.x, pInst.y, pInst.z)
  }

  override def getGeneric(inst: PropertyContainer): Any = get(inst)
  override def setGeneric(inst: PropertyContainer, value: Any): Unit = set(inst, value.asInstanceOf[Vector3])

  def get(inst: PropertyContainer): Vector3
  def set(inst: PropertyContainer, value: Vector3): Unit

  override def writeToBinary(inst: PropertyContainer, buf: ByteBuffer): Unit = {
    val v = get(inst)
    buf.putDouble(v.x)
    buf.putDouble(v.y)
    buf.putDouble(v.z)
  }
  override def readFromBinary(inst: PropertyContainer, buf: ByteBuffer): Unit = {
    val x = buf.getDouble()
    val y = buf.getDouble()
    val z = buf.getDouble()
    set(inst, Vector3(x, y, z))
  }
  override def sizeInBytes: Int = 24
}

