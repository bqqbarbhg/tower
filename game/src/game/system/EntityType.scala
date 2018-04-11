package game.system

import java.nio.ByteBuffer

import scala.collection.mutable.ArrayBuffer
import core._
import game.component.{Component, ComponentType}
import io.serialization.UnstructuredBinaryReader
import util.BufferUtils._

object EntityType {

  def loadFromBuffer(name: String, buffer: ByteBuffer): EntityType = {
    val MaxVersion = 1
    buffer.verifyMagic("s2es")
    val version = buffer.getVersion(MaxVersion)

    val numComponents = buffer.getInt()

    val reader = new UnstructuredBinaryReader(buffer)
    val components = Array.fill(numComponents) {
      val name = buffer.getString()
      val compType = ComponentType.Types.find(_.name == name).getOrElse {
        throw new RuntimeException(s"Unknown component type: $name")
      }

      val component = compType.make
      reader.read(component)
      component
    }

    buffer.verifyMagic("E.es")

    new EntityType(true, name, components)
  }

}

class EntityType(val static: Boolean, val name: String, componentSet: Iterable[Component]) {

  /** Components defining this entity sorted in dependency order */
  val components: Array[Component] = {
    var remaining = componentSet.toBuffer
    var serialized = new ArrayBuffer[Component](remaining.length)

    def canAddHere(component: Component): Boolean = {
      !component.dependencies.exists(compType => remaining.exists(_.componentType == compType))
    }

    while (remaining.nonEmpty) {
      val index = remaining.indexWhere(canAddHere)
      assert(index >= 0, "Cyclical dependency in components")
      serialized += remaining(index)
      remaining(index) = remaining.last
      remaining.trimEnd(1)
    }

    serialized.toArray
  }

}

