package io

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

trait SimpleVisitor {
  def field(v: SimpleVisitor, name: String, value: Int): Int
  def field(v: SimpleVisitor, name: String, value: Long): Long
  def field(v: SimpleVisitor, name: String, value: Float): Float
  def field(v: SimpleVisitor, name: String, value: Double): Double
  def field(v: SimpleVisitor, name: String, value: String): String
  def field(v: SimpleVisitor, name: String, value: Boolean): Boolean
  def field[T <: SimpleSerializable : ClassTag](v: SimpleVisitor, name: String, value: T): T
  def field[T <: SimpleSerializable : ClassTag](v: SimpleVisitor, name: String, value: ArrayBuffer[T], ctor: => T): ArrayBuffer[T]
}

abstract class SimpleReadVisitor extends SimpleVisitor {
  def readField(v: SimpleVisitor, name: String, value: Int): Unit
  def readField(v: SimpleVisitor, name: String, value: Long): Unit
  def readField(v: SimpleVisitor, name: String, value: Float): Unit
  def readField(v: SimpleVisitor, name: String, value: Double): Unit
  def readField(v: SimpleVisitor, name: String, value: String): Unit
  def readField(v: SimpleVisitor, name: String, value: Boolean): Unit
  def readField[T <: SimpleSerializable : ClassTag](v: SimpleVisitor, name: String, value: T): Unit
  def readField[T <: SimpleSerializable : ClassTag](v: SimpleVisitor, name: String, value: ArrayBuffer[T], ctor: => T): Unit

  def field(v: SimpleVisitor, name: String, value: Int): Int = { readField(v, name, value); value }
  def field(v: SimpleVisitor, name: String, value: Long): Long = { readField(v, name, value); value }
  def field(v: SimpleVisitor, name: String, value: Float): Float = { readField(v, name, value); value }
  def field(v: SimpleVisitor, name: String, value: Double): Double = { readField(v, name, value); value }
  def field(v: SimpleVisitor, name: String, value: String): String = { readField(v, name, value); value }
  def field(v: SimpleVisitor, name: String, value: Boolean): Boolean = { readField(v, name, value); value }
  def field[T <: SimpleSerializable : ClassTag](v: SimpleVisitor, name: String, value: T): T = { readField(v, name, value); value }
  def field[T <: SimpleSerializable : ClassTag](v: SimpleVisitor, name: String, value: ArrayBuffer[T], ctor: => T): ArrayBuffer[T] = { readField(v, name, value, ctor); value }
}

trait SimpleSerializable {
  def visit(v: SimpleVisitor): Unit
}
