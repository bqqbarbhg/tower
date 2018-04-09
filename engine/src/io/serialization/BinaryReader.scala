package io.serialization

import java.nio.ByteBuffer

import core._
import util.BufferUtils._

import scala.collection.mutable
import io.property._
import io.serialization.BinaryReader.Prototype

object BinaryReader {

  class Prototype {
    var name: String = ""
    var index: Int = 0
    var numProps: Int = 0
    var propName: Array[String] = null
    var propIndex: Array[Int] = null
    var propSize: Array[Int] = null
    var initialized: Boolean = false
  }

}

class BinaryReader(val buffer: ByteBuffer) {
  val prototypeMapping: Array[Prototype] = {
    val MaxVersion = 1
    buffer.verifyMagic("s2bi")
    val version = buffer.getVersion(MaxVersion)

    val num = buffer.getInt()
    val protos = Array.fill(num)(new Prototype)
    for ((proto, index) <- protos.zipWithIndex) {
      proto.name = buffer.getString()
      proto.index = index
      proto.numProps = buffer.getInt()
      proto.propName = Array.fill(proto.numProps)("")
      proto.propIndex = Array.fill(proto.numProps)(-1)
      proto.propSize = Array.fill(proto.numProps)(-1)
      for (propIx <- 0 until proto.numProps) {
        proto.propName(propIx) = buffer.getString()
        proto.propSize(propIx) = buffer.getInt()
      }
    }

    protos
  }
  val protoByName = prototypeMapping.map(p => (p.name, p)).toMap

  def skip(): Unit = {
    val propIx = buffer.getInt()
    val proto = prototypeMapping(propIx)
    for (ix <- 0 until proto.numProps) {
      buffer.skip(proto.propSize(ix))
    }
  }

  def read(obj: PropertyContainer): Unit = {
    val proto = protoByName.get(obj.propertySet.name).getOrElse {
      throw new RuntimeException(s"Prototype not found in stream: ${obj.propertySet.name}")
    }

    val propIx = buffer.getInt()
    assert(propIx == proto.index)

    val propSet = obj.propertySet.properties
    if (!proto.initialized) {
      for (ix <- 0 until proto.numProps) {
        val name = proto.propName(ix)
        proto.propIndex(ix) = propSet.indexWhere(_.name == name)
      }
      proto.initialized = true
    }

    var ix = 0
    while (ix < proto.numProps) {
      val propIx = proto.propIndex(ix)

      if (propIx >= 0) {
        propSet(propIx) match {
          case b: BinarySerializable => b.readFromBinary(obj, buffer)
          case p: ProductProp =>
            val inst = p.makeProductInstance
            read(inst)
            p.setProductInstance(obj, inst)
        }
      } else {
        buffer.skip(proto.propSize(ix))
      }

      ix += 1
    }
  }

}
