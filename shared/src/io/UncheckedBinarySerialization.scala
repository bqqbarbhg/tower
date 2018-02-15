package io

import java.nio.ByteBuffer
import collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
  * _Unchecked_ binary serialization reader, depends on all fields being the same
  * and in the same order as they were when serializing. Use external check to see
  * if data is valid.
  */
class UncheckedBinaryReader(buf: ByteBuffer) extends SimpleVisitor {
  def field(v: SimpleVisitor, name: String, value: Int): Int =  buf.getInt()
  def field(v: SimpleVisitor, name: String, value: Long): Long = buf.getLong()
  def field(v: SimpleVisitor, name: String, value: Float): Float = buf.getFloat()
  def field(v: SimpleVisitor, name: String, value: Double): Double = buf.getDouble()
  def field(v: SimpleVisitor, name: String, value: String): String = {
    val len = buf.getInt()
    val arr = new Array[Char](len)
    buf.asCharBuffer.get(arr)
    buf.position(buf.position + len * 2)
    if (len % 2 != 0) buf.getChar() // Align to 4 bytes
    new String(arr)
  }
  def field(v: SimpleVisitor, name: String, value: Boolean): Boolean = buf.getInt() != 0
  def field[T <: SimpleSerializable : ClassTag](v: SimpleVisitor, name: String, value: T): T = {
    value.visit(this)
    value
  }
  def field[T <: SimpleSerializable : ClassTag](v: SimpleVisitor, name: String, value: ArrayBuffer[T], ctor: => T): ArrayBuffer[T] = {
    val num = buf.getInt()
    for (i <- 0 until num) {
      val inst = ctor
      inst.visit(this)
      value += inst
    }
    value
  }
}

/**
  * _Unchecked_ binary serialization writer, writes the field contents directly
  * to memory without any metadata or field names. Use external validation to
  * make sure no invalid data is read.
  */
class UncheckedBinaryWriter(buf: ByteBuffer) extends SimpleReadVisitor {
  def readField(v: SimpleVisitor, name: String, value: Int): Unit = buf.putInt(value)
  def readField(v: SimpleVisitor, name: String, value: Long): Unit = buf.putLong(value)
  def readField(v: SimpleVisitor, name: String, value: Float): Unit = buf.putFloat(value)
  def readField(v: SimpleVisitor, name: String, value: Double): Unit = buf.putDouble(value)
  def readField(v: SimpleVisitor, name: String, value: String): Unit = {
    buf.putInt(value.length)
    buf.asCharBuffer.put(value.toCharArray)
    buf.position(buf.position + value.length * 2)
    if (value.length % 2 != 0) buf.putChar(0) // Align to 4 bytes
  }
  def readField(v: SimpleVisitor, name: String, value: Boolean): Unit = buf.putInt(if (value) 1 else 0)
  def readField[T <: SimpleSerializable : ClassTag](v: SimpleVisitor, name: String, value: T): Unit = {
    value.visit(this)
  }
  def readField[T <: SimpleSerializable : ClassTag](v: SimpleVisitor, name: String, value: ArrayBuffer[T], ctor: => T): Unit = {
    buf.putInt(value.length)
    for (v <- value) {
      v.visit(this)
    }
  }
}
