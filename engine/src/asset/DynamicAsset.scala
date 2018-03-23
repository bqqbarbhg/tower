package asset

import core._

object DynamicAsset {
  def apply[T <: Unloadable](name: String, ctor: => T): DynamicAsset[T] = apply(Identifier(name), ctor)
  def apply[T <: Unloadable](name: Identifier, ctor: => T): DynamicAsset[T] = new DynamicAsset[T](name, () => ctor)
}

class DynamicAsset[T <: Unloadable](val name: Identifier, val ctor: () => T) extends LoadableAsset {
  def debugName: String = s"Dynamic: $name"

  def this(name: String, ctor: => T) = this(Identifier(name), () => ctor)

  private var impl: T = _

  def get: T = {
    load()
    impl
  }

  override def loadAsset(): Unit = {
    impl = ctor()
  }

  override def unloadAsset(): Unit = {
    impl.unload()
  }
}

