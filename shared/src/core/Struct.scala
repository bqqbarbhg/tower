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

  class FieldInt extends Field {
    def get(buf: ByteBuffer, base: Int): Int = buf.getInt(base + offset)
    def set(buf: ByteBuffer, base: Int, value: Int) = buf.putInt(base + offset, value)

    def size: Int = 4
    def align: Int = 4
  }

}

class Struct {
  private var loc = 0
  private var mutAlign = 1
  private var mutSize = 0

  def size: Int = mutSize
  def align: Int = mutAlign

  def push[T <: Field](field: T): T = {
    val fieldAlign = field.align
    val alignedLoc = loc + (fieldAlign - loc % fieldAlign) % fieldAlign
    field.offset = loc
    loc += field.size
    mutAlign = math.max(mutAlign, field.align)
    mutSize = loc + (mutAlign - loc % mutAlign) % mutAlign
    field
  }

  def int: FieldInt = push(new FieldInt())

}

