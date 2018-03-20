package asset

import core._
import gfx.ShaderSource
import gfx.ShaderSource._
import render.SamplerBlock.NoSamplers
import render.{SamplerBlock, UniformBlock}

import scala.collection.mutable.ArrayBuffer

object ShaderSourceAsset {
  def apply(name: String): ShaderSourceAsset = apply(Identifier(name))
  def apply(name: Identifier): ShaderSourceAsset = AssetLoader.getOrAdd(name, new ShaderSourceAsset(name))
}

class ShaderSourceAsset(val name: Identifier) extends LoadableAsset {
  def debugName: String = s"Shader source: $name"

  def this(name: String) = this(Identifier(name))

  private var sourceImpl: ShaderSource = null

  def get: ShaderSource = {
    load()
    sourceImpl
  }

  override def preloadAsset(): Iterable[LoadableAsset] = {
    sourceImpl = ShaderSource.load(name).get
    sourceImpl.imports
  }

  override def loadAsset(): Unit = {
  }

  override def unloadAsset(): Unit = {
    sourceImpl = null
  }
}

