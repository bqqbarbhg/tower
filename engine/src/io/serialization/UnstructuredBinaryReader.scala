package io.serialization

import java.nio.ByteBuffer

import core._
import io.property.{ProductProp, PropertyContainer}
import util.BufferUtils._
import UnstructuredBinaryReader._

object UnstructuredBinaryReader {
  val DataDouble = 1
  val DataInt = 2
  val DataString = 3
  val DataObject = 4
}

class UnstructuredBinaryReader(val buffer: ByteBuffer) extends AnyVal {

  def read(obj: PropertyContainer): Unit = {
    val props = obj.propertySet
    val numFields = buffer.getInt()
    var ix = 0
    while (ix < numFields) {
      val name = buffer.getString()
      val dataType = buffer.getInt()
      props.properties.find(_.name == name) match {
        case Some(prop) =>

          props.customConverters.get(name) match {
            case Some(conv) =>
              dataType match {
                case DataDouble => conv.set(obj, prop, buffer.getDouble())
                case DataInt => conv.set(obj, prop, buffer.getInt())
                case DataString => conv.set(obj, prop, buffer.getString())
              }

            case None =>
              dataType match {
                case DataDouble => prop.setGenericWithConversion(obj, buffer.getDouble())
                case DataInt => prop.setGenericWithConversion(obj, buffer.getInt())
                case DataString => prop.setGenericWithConversion(obj, buffer.getString())
                case DataObject =>
                  val product = prop.asInstanceOf[ProductProp]
                  val productObj = product.getProductInstance(obj)
                  read(productObj)
                  product.setProductInstance(obj, productObj)
              }
          }


        case None => skipType(dataType)
      }
      ix += 1
    }
  }

  def skipObject(): Unit = {
    val numFields = buffer.getInt()
    var ix = 0
    while (ix < numFields) {
      val name = buffer.getString()
      val dataType = buffer.getInt()
      skipType(dataType)
      ix += 1
    }
  }

  def skipType(dataType: Int): Unit = dataType match {
    case DataDouble => buffer.getDouble()
    case DataInt => buffer.getInt()
    case DataString => buffer.getString()
    case DataObject => skipObject()
  }

}

