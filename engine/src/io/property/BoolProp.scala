package io.property

import java.nio.ByteBuffer

import io.serialization.BinarySerializable

object BoolProp {
  type Type = Boolean
}

abstract class BoolProp(name: String) extends Property(name) with BinarySerializable {
  override def getGeneric(inst: PropertyContainer): Any = get(inst)
  override def setGeneric(inst: PropertyContainer, value: Any): Unit = set(inst, value.asInstanceOf[Boolean])

  def get(inst: PropertyContainer): Boolean
  def set(inst: PropertyContainer, value: Boolean): Unit

  override def writeToBinary(inst: PropertyContainer, buf: ByteBuffer): Unit = {
    buf.put(if (get(inst)) 1.toByte else 0.toByte)
  }
  override def readFromBinary(inst: PropertyContainer, buf: ByteBuffer, offset: Int): Unit = {
    set(inst, buf.get(offset) != 0)
  }
}

