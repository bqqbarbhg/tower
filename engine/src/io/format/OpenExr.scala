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

    def puts(buf: ByteBuffer, str: String): Unit = {
      val chars = str.toCharArray
      for (c <- chars) {
        buf.put(c.toByte)
      }
      buf.put(0.toByte)
    }

    def putAttrib(name: String, typ: String, write: ByteBuffer => Unit): Unit = {
      tempBytes.position(0)
      write(tempBytes)
      val num = tempBytes.position
      tempBytes.position(0)
      val slice = tempBytes.sliceEx
      slice.limit(num)
      puts(dst, name)
      puts(dst, typ)
      dst.putInt(num)
      dst.put(slice)
    }

    dst.putInt(20000630)

    val version = 2
    var flags = 0x0
    dst.putInt(version | flags)

    putAttrib("channels", "chlist", b => {
      for (ch <- Array("B", "G", "R")) {
        puts(b, ch)
        b.putInt(2)
        b.put(1.toByte)
        b.put(0.toByte)
        b.put(0.toByte)
        b.put(0.toByte)
        b.putInt(1)
        b.putInt(1)
      }

      b.put(0.toByte)
    })

    putAttrib("compression", "compression", b => {
      b.put(0.toByte)
    })

    putAttrib("dataWindow", "box2i", b => {
      b.putInt(0)
      b.putInt(0)
      b.putInt(width - 1)
      b.putInt(height - 1)
    })

    putAttrib("displayWindow", "box2i", b => {
      b.putInt(0)
      b.putInt(0)
      b.putInt(width - 1)
      b.putInt(height - 1)
    })

    putAttrib("lineOrder", "lineOrder", b => {
      b.put(0.toByte)
    })

    putAttrib("pixelAspectRatio", "float", b => {
      b.putFloat(1.0f)
    })

    putAttrib("screenWindowCenter", "v2f", b => {
      b.putFloat(0.0f)
      b.putFloat(0.0f)
    })

    putAttrib("screenWindowWidth", "float", b => {
      b.putFloat(1.0f)
    })

    dst.put(0.toByte)

    val OffsetSize = 8
    val LineSize = width * 3 * 4
    val ChunkSize = (4 + 4 + LineSize)
    val dataBegin = dst.position + height * OffsetSize

    for (i <- 0 until height) {
      dst.putLong(dataBegin + i * ChunkSize)
    }

    for (y <- 0 until height) {
      dst.putInt(y)
      dst.putInt(LineSize)

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

