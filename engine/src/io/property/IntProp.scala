package io.property

import java.nio.ByteBuffer

import io.serialization.BinarySerializable

object IntProp {
  type Type = Int
}

abstract class IntProp(name: String) extends Property(name) with BinarySerializable {
  override def getGeneric(inst: PropertyContainer): Any = get(inst)
  override def setGeneric(inst: PropertyContainer, value: Any): Unit = set(inst, value.asInstanceOf[Int])
  override def setGenericWithConversion(inst: PropertyContainer, value: Any): Unit = value match {
    case v: Int => set(inst, v)
    case v: Double => set(inst, v.toInt)
  }

  def get(inst: PropertyContainer): Int
  def set(inst: PropertyContainer, value: Int): Unit

  override def writeToBinary(inst: PropertyContainer, buf: ByteBuffer): Unit = {
    buf.putInt(get(inst))
  }
  override def readFromBinary(inst: PropertyContainer, buf: ByteBuffer): Unit = {
    set(inst, buf.getInt())
  }
  override def sizeInBytes: Int = 4
}

