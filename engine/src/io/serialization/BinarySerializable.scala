package io.serialization

import io.property.PropertyContainer

import java.nio.ByteBuffer

trait BinarySerializable {

  def writeToBinary(inst: PropertyContainer, buf: ByteBuffer): Unit
  def readFromBinary(inst: PropertyContainer, buf: ByteBuffer, offset: Int): Unit

}

