package io.serialization

import java.nio.ByteBuffer
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
        case p: ProductProp =>
          p.makeProductInstance
      }
    }
  }

}

