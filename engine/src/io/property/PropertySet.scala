package io.property

import scala.collection.mutable

class PropertySet(val name: String, props: Seq[Property], val parent: Option[PropertySet] = None) {

  def this(name: String, props: Seq[Property], parent: PropertySet) = this(name, props, Some(parent))

  val properties: Array[Property] = parent match {
    case Some(p) => (p.properties ++ props).toArray
    case None => props.toArray
  }

  val enumerations: mutable.HashMap[String, Seq[Any]] = parent.map(_.enumerations).getOrElse(mutable.HashMap[String, Seq[Any]]())
  val ranges: mutable.HashMap[String, (Any, Any)] = parent.map(_.ranges).getOrElse(mutable.HashMap[String, (Any, Any)]())

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

}

