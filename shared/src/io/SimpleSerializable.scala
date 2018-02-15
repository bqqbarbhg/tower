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

trait SimpleSerializable {
  def visit(v: SimpleVisitor): Unit
}
