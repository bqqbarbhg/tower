package io.serialization

import java.nio.ByteBuffer

import util.BufferUtils._
import scala.collection.mutable
import io.property._

class BinaryWriter(val buffer: ByteBuffer) {
  private val prototypes = new mutable.HashMap[PropertySet, Int]()

  def write(obj: PropertyContainer): Unit = {
    val propSet = obj.propertySet
    val index = prototypes.getOrElseUpdate(propSet, prototypes.size)
    buffer.putInt(index)
    for (prop <- propSet.properties) {
      prop match {
        case b: BinarySerializable => b.writeToBinary(obj, buffer)
        case p: ProductProp => write(p.getProductInstance(obj))
      }
    }
  }

  private def propSizeInBytes(prop: Property): Int = {
    prop match {
      case b: BinarySerializable => b.sizeInBytes
      case p: ProductProp => p.propertySet.properties.map(propSizeInBytes).sum
    }
  }

  def writeHeader(header: ByteBuffer): Unit = {
    val protos = prototypes.toSeq.sortBy(_._2).map(_._1)
    val Version = 1
    header.putMagic("s2bi")
    header.putVersion(Version)
    header.putInt(protos.length)
    for (proto <- protos) {
      header.putString(proto.name)
      header.putInt(proto.properties.length)
      for (prop <- proto.properties) {
        header.putString(prop.name)
        header.putInt(propSizeInBytes(prop))
      }
    }
  }

}

