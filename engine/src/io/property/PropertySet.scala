package io.property

import scala.collection.mutable

class PropertySet(props: Seq[Property]) {
  val properties: Array[Property] = props.toArray
  val enumerations: mutable.HashMap[String, Seq[Any]] = mutable.HashMap[String, Seq[Any]]()

  def enum(name: String, values: Seq[Any]) = enumerations(name) = values

}

