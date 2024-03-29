package io.property

import java.nio.ByteBuffer

import io.serialization.BinarySerializable

object DoubleProp {
  type Type = Double
}

abstract class DoubleProp(name: String) extends Property(name) with BinarySerializable {
  override def getGeneric(inst: PropertyContainer): Any = get(inst)
  override def setGeneric(inst: PropertyContainer, value: Any): Unit = set(inst, value.asInstanceOf[Double])
  override def setGenericWithConversion(inst: PropertyContainer, value: Any): Unit = value match {
    case v: Double => set(inst, v)
    case v: Int => set(inst, v.toDouble)
  }

  def get(inst: PropertyContainer): Double
  def set(inst: PropertyContainer, value: Double): Unit

  override def writeToBinary(inst: PropertyContainer, buf: ByteBuffer): Unit = {
    buf.putDouble(get(inst))
  }
  override def readFromBinary(inst: PropertyContainer, buf: ByteBuffer): Unit = {
    set(inst, buf.getDouble())
  }
  override def sizeInBytes: Int = 8
}

