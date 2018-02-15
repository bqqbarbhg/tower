package io

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

trait SimpleVisitor {
  def field(name: String, value: Int): Int
  def field(name: String, value: Long): Long
  def field(name: String, value: Float): Float
  def field(name: String, value: Double): Double
  def field(name: String, value: String): String
  def field(name: String, value: Boolean): Boolean
  def field[T <: SimpleSerializable : ClassTag](name: String, value: T): T
  def field[T <: SimpleSerializable : ClassTag](name: String, value: ArrayBuffer[T], ctor: => T): ArrayBuffer[T]
}

abstract class SimpleReadVisitor extends SimpleVisitor {
  def readField(name: String, value: Int): Unit
  def readField(name: String, value: Long): Unit
  def readField(name: String, value: Float): Unit
  def readField(name: String, value: Double): Unit
  def readField(name: String, value: String): Unit
  def readField(name: String, value: Boolean): Unit
  def readField[T <: SimpleSerializable : ClassTag](name: String, value: T): Unit
  def readField[T <: SimpleSerializable : ClassTag](name: String, value: ArrayBuffer[T], ctor: => T): Unit

  def field(name: String, value: Int): Int = { readField(name, value); value }
  def field(name: String, value: Long): Long = { readField(name, value); value }
  def field(name: String, value: Float): Float = { readField(name, value); value }
  def field(name: String, value: Double): Double = { readField(name, value); value }
  def field(name: String, value: String): String = { readField(name, value); value }
  def field(name: String, value: Boolean): Boolean = { readField(name, value); value }
  def field[T <: SimpleSerializable : ClassTag](name: String, value: T): T = { readField(name, value); value }
  def field[T <: SimpleSerializable : ClassTag](name: String, value: ArrayBuffer[T], ctor: => T): ArrayBuffer[T] = { readField(name, value, ctor); value }
}

trait SimpleSerializable {
  def visit(v: SimpleVisitor): Unit
}
