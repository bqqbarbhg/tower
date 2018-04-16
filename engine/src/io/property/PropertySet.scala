package io.property

import scala.collection.mutable
import java.nio.ByteBuffer

import PropertySet._

object PropertySet {

  trait CustomConverter {
    def set(inst: PropertyContainer, prop: Property, value: Int): Unit = prop.setGenericWithConversion(inst, value)
    def set(inst: PropertyContainer, prop: Property, value: Double): Unit = prop.setGenericWithConversion(inst, value)
    def set(inst: PropertyContainer, prop: Property, value: String): Unit = prop.setGenericWithConversion(inst, value)
  }

}

class PropertySet(val name: String, props: Seq[Property], val parent: Option[PropertySet] = None) {

  def this(name: String, props: Seq[Property], parent: PropertySet) = this(name, props, Some(parent))

  val properties: Array[Property] = parent match {
    case Some(p) => (p.properties ++ props).toArray
    case None => props.toArray
  }


  val enumerations: mutable.HashMap[String, Seq[Any]] = parent.map(_.enumerations).getOrElse(mutable.HashMap[String, Seq[Any]]())
  val ranges: mutable.HashMap[String, (Any, Any)] = parent.map(_.ranges).getOrElse(mutable.HashMap[String, (Any, Any)]())
  val customConverters: mutable.HashMap[String, CustomConverter] = parent.map(_.customConverters).getOrElse(mutable.HashMap[String, CustomConverter]())

  def enum(name: String, values: Seq[Any]): Unit = {
    val prefix = name.split('.')(0)
    require(properties.exists(_.name == prefix))
    enumerations(name) = values
  }

  def range(name: String, min: Any, max: Any): Unit = {
    val prefix = name.split('.')(0)
    require(properties.exists(_.name == prefix))
    ranges(name) = (min, max)
  }

  def customConverter(name: String, converter: CustomConverter): Unit = {
    val prefix = name.split('.')(0)
    require(properties.exists(_.name == prefix))
    customConverters(prefix) = converter
  }

}

