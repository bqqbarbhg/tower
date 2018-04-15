package io.format

import core._
import java.nio.{ByteBuffer, ByteOrder}

import scala.collection.mutable.ArrayBuffer

object OpenExr {

  def writeLinearOpenExrFloat32(dst: ByteBuffer, data: ByteBuffer, width: Int, height: Int): Unit = {
    val pixelPitch = 4 * 3
    writeLinearOpenExrFloat32(dst, data, width, height, width * pixelPitch, pixelPitch)
  }

  def writeLinearOpenExrFloat32(dst: ByteBuffer, data: ByteBuffer, width: Int, height: Int, scanLinePitch: Int, pixelPitch: Int): Unit = {

    val tempBytes = Memory.alloc(1024)

    /** Write zero terminated string `str` to `buf` */
    def puts(buf: ByteBuffer, str: String): Unit = {
      val chars = str.toCharArray
      for (c <- chars) {
        buf.put(c.toByte)
      }
      buf.put(0.toByte)
    }

    /**
      * Write OpenEXR header attribute. Attributes consist of name, type,
      * value size, and type-specific value data.
      */
    def putAttrib(name: String, typ: String, write: ByteBuffer => Unit): Unit = {

      // Write the attribute value into a temporary buffer
      tempBytes.position(0)
      write(tempBytes)
      val valueSize = tempBytes.position
      tempBytes.position(0)
      val slice = tempBytes.sliceEx
      slice.limit(num)

      puts(dst, name)       // Attribute name
      puts(dst, typ)        // Attribute type
      dst.putInt(valueSize) // Value size
      dst.put(slice)        // Value data (from temporary buffer)
    }

    // -- Components One and Two: Magic Number and Version Field

    // Magic Number
    // The magic number, of type int, is always 20000630 (decimal).
    dst.putInt(20000630)

    // Version Field
    // The version field, of type int, is the four-byte group following the magic number,
    // and it is treated as two separate bit fields.
    // The 8 least significant bits, they contain the file format version number.
    // The current OpenEXR version number is version 2.
    // The 24 most significant bits, these are treated as a set of boolean flags.
    // - No flags are necessary for a simple non-multipart non-chunked image
    val version = 2
    var flags = 0x0
    dst.putInt(version | flags)

    // -- Component Three: Header
    // The header component of the single-part file holds a single header (for single-part files).
    // Each header is a sequence of attributes ended by a null byte.

    putAttrib("channels", "chlist", b => {
      for (ch <- Array("B", "G", "R")) {
        puts(b, ch)     // Channel name
        b.putInt(2)     // Format: FLOAT
        b.put(1.toByte) // Linear: TRUE
        b.put(0.toByte) // Reserved
        b.put(0.toByte) // Reserved
        b.put(0.toByte) // Reserved
        b.putInt(1)     // Subsamples X: 1
        b.putInt(1)     // Subsamples Y: 1
      }

      // Null-terminator for channels
      b.put(0.toByte)
    })

    putAttrib("compression", "compression", b => {
      b.put(0.toByte) // NO_COMPRESSION
    })

    putAttrib("dataWindow", "box2i", b => {
      b.putInt(0)          // minX (inclusive)
      b.putInt(0)          // minY (inclusive)
      b.putInt(width - 1)  // maxX (inclusive)
      b.putInt(height - 1) // maxY (inclusive)
    })

    putAttrib("displayWindow", "box2i", b => {
      b.putInt(0)          // minX (inclusive)
      b.putInt(0)          // minY (inclusive)
      b.putInt(width - 1)  // maxX (inclusive)
      b.putInt(height - 1) // maxY (inclusive)
    })

    putAttrib("lineOrder", "lineOrder", b => {
      b.put(0.toByte)      // INCREASING_Y
    })

    putAttrib("pixelAspectRatio", "float", b => {
      b.putFloat(1.0f)
    })

    // Programs that deal with images as purely two-dimensional objects
    // may not be able to generate a description of a perspective
    // projection. Those programs should set screenWindowWidth to 1, and
    // screenWindowCenter to (0, 0).

    putAttrib("screenWindowCenter", "v2f", b => {
      b.putFloat(0.0f)
      b.putFloat(0.0f)
    })

    putAttrib("screenWindowWidth", "float", b => {
      b.putFloat(1.0f)
    })

    // Null-terminator for header attributes
    dst.put(0.toByte)

    // -- Component Four: Offset Tables

    // An offset table allows random access to pixel data chunks. An
    // offset table is a sequence of offsets, with one offset per chunk.
    // Each offset (of type unsigned long) indicates the distance, in
    // bytes, between the start of the file and the start of the chunk.

    val OffsetSize = 8
    val LineSize = width * 3 * 4
    val ChunkSize = (4 + 4 + LineSize)
    val dataBegin = dst.position + height * OffsetSize

    for (i <- 0 until height) {
      dst.putLong(dataBegin + i * ChunkSize)
    }

    // -- Component Five: Pixel data

    for (y <- 0 until height) {

      // Regular scan line image block layout

      dst.putInt(y)        // Y-coordinate
      dst.putInt(LineSize) // Pixel data size

      // Pixel data
      // Within the pixel data, scan lines are stored top to bottom.
      // Each scan line is contiguous, and within a scan line the data
      // for each channel are contiguous. Channels are stored in
      // alphabetical order, according to channel names. Within a channel,
      // pixels are stored left to right.

      val base = (height - y - 1) * scanLinePitch

      var x = 0
      while (x < width) {
        val ix = base + x * pixelPitch
        dst.putFloat(data.getFloat(ix + 8))
        x += 1
      }

      x = 0
      while (x < width) {
        val ix = base + x * pixelPitch
        dst.putFloat(data.getFloat(ix + 4))
        x += 1
      }

      x = 0
      while (x < width) {
        val ix = base + x * pixelPitch
        dst.putFloat(data.getFloat(ix + 0))
        x += 1
      }
    }

    Memory.free(tempBytes)

  }

}

