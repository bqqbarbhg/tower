package game.component

import game.system.Entity
import io.property._
import asset._

abstract class Component extends PropertyContainer {

  def componentType: ComponentType

  def create(entity: Entity): Unit = { }
  def dependencies: Iterable[ComponentType] = None
  def assets: Iterable[LoadableAsset] = None

}

