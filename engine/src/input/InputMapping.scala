package input

import scala.collection.mutable
import core._
import input.device.InputDevice
import io.SimpleSerialization.{SMap, SString}

import scala.collection.mutable.ArrayBuffer

object InputMapping {

}

class InputMapping(val devices: Array[InputDevice]) {
  var elements = Map[IdentifierIx, Array[Int]]()

  def init(mapping: SMap): Unit = {
    var tempElements = new mutable.HashMap[Identifier, ArrayBuffer[Int]]()

    for {
      (deviceType, allBinds) <- mapping.pairs
      (bindSetName, bindSet) <- allBinds.asInstanceOf[SMap].pairs
      (bindName, elementName) <- bindSet.asInstanceOf[SMap].pairs
      (device, deviceIndex) <- devices.filter(_.deviceType.name == deviceType).zipWithIndex
      elementIndex <- device.nameToButtonIndex(elementName.asInstanceOf[SString].v)
    } {

      val id = Identifier(bindSetName + "." + bindName)
      val list = tempElements.getOrElseUpdate(id, new ArrayBuffer[Int]())

      list += deviceIndex
      list += elementIndex
    }

    elements = tempElements.map(p => (p._1.index, p._2.toArray)).toMap

  }

  def isDown(button: InputSet.Button): Boolean = {
    val array = elements(button.name.index)
    var ix = 0
    val num = array.length
    while (ix < num) {
      val device = array(ix + 0)
      val element = array(ix + 1)
      if (devices(device).isButtonDown(element)) return true
      ix += 2
    }
    false
  }

  def wasDown(button: InputSet.Button): Boolean = {
    val array = elements(button.name.index)
    var ix = 0
    val num = array.length
    while (ix < num) {
      val device = array(ix + 0)
      val element = array(ix + 1)
      if (devices(device).wasButtonDown(element)) return true
      ix += 2
    }
    false
  }

  def justPressed(button: InputSet.Button): Boolean = isDown(button) && !wasDown(button)

}

