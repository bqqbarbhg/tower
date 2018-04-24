package asset

import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer

import core._
import game.system._
import game.component._
import io.ContentFile
import task.Task

object EntityTypeAsset {
  def apply(name: String): EntityTypeAsset = if (name.endsWith(".s2es")) apply(Identifier(name)) else apply(Identifier(name + ".s2es"))
  def apply(name: Identifier): EntityTypeAsset = AssetLoader.getOrAdd(name, new EntityTypeAsset(name))
}

class EntityTypeAsset(val name: Identifier) extends LoadableAsset {
  def debugName: String = s"Entity type: $name"

  def this(name: String) = this(Identifier(name))

  private var entityType: EntityType = null

  override protected def preloadAsset(): Iterable[LoadableAsset] = {
    entityType = ContentFile.load(name, buffer => {
      EntityType.loadFromBuffer(name.toString, buffer, Some(this))
    }).get

    entityType.components.flatMap(_.assets)
  }

  def get: EntityType = {
    load()
    entityType
  }

  /**
    * Returns a model instance with potentially only metadata loaded.
    */
  def getShallowUnsafe: EntityType = {
    preload()
    entityType
  }

  override def loadAsset(): Unit = {
  }

  override def unloadAsset(): Unit = {
    entityType = null
  }
}

