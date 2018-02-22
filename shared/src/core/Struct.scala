package core

import java.nio.ByteBuffer

import core.Struct._

object Struct {

  abstract class Field {
    /** Offset of the field in bytes */
    var offset: Int = 0
    /** Size of the field in bytes */
    def size: Int
    /** Alignment of the field in bytes */
    def align: Int
  }

  class FieldByte extends Field {
    def get(buf: ByteBuffer, base: Int): Byte = buf.get(base + offset)
    def set(buf: ByteBuffer, base: Int, value: Byte) = buf.put(base + offset, value)

    def size: Int = 1
    def align: Int = 1
  }

  class FieldShort extends Field {
    def get(buf: ByteBuffer, base: Int): Short = buf.getShort(base + offset)
    def set(buf: ByteBuffer, base: Int, value: Short) = buf.putShort(base + offset, value)

    def size: Int = 2
    def align: Int = 2
  }

  class FieldInt extends Field {
    def get(buf: ByteBuffer, base: Int): Int = buf.getInt(base + offset)
    def set(buf: ByteBuffer, base: Int, value: Int) = buf.putInt(base + offset, value)

    def size: Int = 4
    def align: Int = 4
  }

  class FieldLong extends Field {
    def get(buf: ByteBuffer, base: Int): Long = buf.getLong(base + offset)
    def set(buf: ByteBuffer, base: Int, value: Long) = buf.putLong(base + offset, value)

    def size: Int = 8
    def align: Int = 8
  }

  class FieldFloat extends Field {
    def get(buf: ByteBuffer, base: Int): Float = buf.getFloat(base + offset)
    def set(buf: ByteBuffer, base: Int, value: Float) = buf.putFloat(base + offset, value)

    def size: Float = 4
    def align: Float = 4
  }

  class FieldDouble extends Field {
    def get(buf: ByteBuffer, base: Int): Double = buf.getDouble(base + offset)
    def set(buf: ByteBuffer, base: Int, value: Double) = buf.putDouble(base + offset, value)

    def size: Int = 8
    def align: Int = 8
  }

}

class Struct {
  private var loc = 0
  private var mutAlign = 1
  private var mutSize = 0

  def size: Int = mutSize
  def align: Int = mutAlign

  protected def push[T <: Field](field: T): T = {
    val fieldAlign = field.align
    val alignedLoc = loc + (fieldAlign - loc % fieldAlign) % fieldAlign
    field.offset = loc
    loc += field.size
    mutAlign = math.max(mutAlign, field.align)
    mutSize = loc + (mutAlign - loc % mutAlign) % mutAlign
    field
  }

  protected def byte: FieldByte = push(new FieldByte())
  protected def short: FieldShort = push(new FieldShort())
  protected def int: FieldInt = push(new FieldInt())
  protected def long: FieldLong = push(new FieldLong())
  protected def float: FieldInt = push(new FieldFloat())
  protected def double: FieldLong = push(new FieldDouble())

}

