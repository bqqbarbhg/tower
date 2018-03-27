package io.format

import core._
import java.nio.{ByteBuffer, ByteOrder}

import scala.collection.mutable.ArrayBuffer

object Tiff {

  case class Field(tag: Int, typ: Int, num: Int, offsetOrImm: Int)

  class FieldSet(val buffer: ByteBuffer) {
    var pos: Int = 0
    val fields = ArrayBuffer[Field]()

    def word(tag: Int, values: Int*): Unit = wordList(tag, values)
    def wordList(tag: Int, values: Seq[Int]): Unit = {
      if (values.length > 2) {
        fields += new Field(tag, 3, values.length, buffer.position)
        for (v <- values) buffer.putShort(v.toShort)
      } else {
        var imm = 0
        if (values.length >= 1) imm |= (values(0) & 0xFFFF)
        if (values.length >= 2) imm |= (values(1) & 0xFFFF) << 16
        fields += new Field(tag, 3, values.length, imm)
      }
    }

    def dword(tag: Int, values: Int*): Unit = dwordList(tag, values)
    def dwordList(tag: Int, values: Seq[Int]): Unit = {
      if (values.length > 1) {
        fields += new Field(tag, 4, values.length, buffer.position)
        for (v <- values) buffer.putInt(v)
      } else {
        var imm = 0
        if (values.length >= 1) imm = values(0)
        fields += new Field(tag, 3, values.length, imm)
      }
    }

    /** Write the image file directory (IFD) and return it's starting offset */
    def writeIfd(): Int = {
      val offset = buffer.position

      buffer.putShort(fields.length.toShort)

      for (field <- fields) {
        buffer.putShort(field.tag.toShort)
        buffer.putShort(field.typ.toShort)
        buffer.putInt(field.num)
        buffer.putInt(field.offsetOrImm)
      }

      // Offset to next IFD (NULL for last)
      buffer.putInt(0)

      offset
    }
  }

  val ImageWidth = 0x0100
  val ImageLength = 0x0101
  val BitsPerSample = 0x0102
  val Compression = 0x0103
  val PhotometricInterpretation = 0x0106
  val StripOffsets = 0x0111
  val Orientation = 0x0112
  val SamplesPerPixel = 0x0115
  val StripByteCounts = 0x0117
  val PlanarConfiguration = 0x011C
  val TransferFunction = 0x012D

  def writeLinearTiffRgb16(buffer: ByteBuffer, data: ByteBuffer, width: Int, height: Int): Unit = {
    writeLinearTiffRgb16(buffer, data, width, height, width * 2 * 3, 2 * 3)
  }

  def writeLinearTiffRgb16(dst: ByteBuffer, data: ByteBuffer, width: Int, height: Int, scanLinePitch: Int, pixelPitch: Int): Unit = {
    val buffer = dst.sliceEx

    // Header: Endianness of rest of the file
    if (buffer.order == ByteOrder.LITTLE_ENDIAN) {
      // Little (Intel)
      buffer.put('I'.toByte)
      buffer.put('I'.toByte)
    } else {
      // Big (Motorola)
      buffer.put('M'.toByte)
      buffer.put('M'.toByte)
    }

    // Version number
    buffer.putShort(42)

    // Offset of the image directory (will be written later)
    val offsetOfDirectoryOffset = buffer.position()
    buffer.putInt(0)

    // Actual image data
    val offsetOfData = buffer.position()
    var y = 0
    while (y < height) {
      var p = y * scanLinePitch
      var x = 0
      while (x < width) {
        buffer.putShort(data.getShort(p + 0))
        buffer.putShort(data.getShort(p + 2))
        buffer.putShort(data.getShort(p + 4))
        x += 1
        p += pixelPitch
      }
      y += 1
    }

    // Image directory
    val fields = new FieldSet(buffer)
    fields.word(PhotometricInterpretation, 3) // RGB
    fields.word(Compression, 1)               // No compression
    fields.word(PlanarConfiguration, 1)       // Interleaved RGB
    fields.word(Orientation, 1)               // Top-left

    fields.word(ImageWidth, width.toShort)
    fields.word(ImageLength, height.toShort)
    fields.word(BitsPerSample, 16, 16, 16)
    fields.word(SamplesPerPixel, 3)

    fields.wordList(TransferFunction, (0x0000 to 0xFFFF))

    fields.dword(StripOffsets, offsetOfData)
    fields.dword(StripByteCounts, width * height * 3 * 2)

    val directoryOffset = fields.writeIfd()
    buffer.putInt(offsetOfDirectoryOffset, directoryOffset)

    dst.position(dst.position + buffer.position)
  }


}

