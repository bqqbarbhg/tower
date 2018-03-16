package asset

import core._
import gfx.Shader
import gfx.Shader._
import render.SamplerBlock.NoSamplers
import render.{SamplerBlock, UniformBlock}

import scala.collection.mutable.ArrayBuffer

class ShaderAsset(val name: Identifier) extends LoadableAsset {
  def this(name: String) = this(Identifier(name))

  private var shaderImpl: Shader = null

  def Textures: SamplerBlock = NoSamplers
  def Permutations: Permutations = NoPermutations
  var uniforms: ArrayBuffer[UniformBlock] = ArrayBuffer[UniformBlock]()

  def uniform(block: UniformBlock): Unit = uniforms += block

  var vertSource = new ShaderSourceAsset(name.toString + ".vs.glsl.s2sh")
  var fragSource = new ShaderSourceAsset(name.toString + ".fs.glsl.s2sh")

  def get: Shader = {
    load()
    shaderImpl
  }

  override def preloadAsset(): Iterable[LoadableAsset] = {
    Array(vertSource, fragSource)
  }

  override def loadAsset(): Unit = {
    shaderImpl = Shader.load(name.toString, vertSource.get, fragSource.get, Permutations, Textures, uniforms : _*)
  }

  override def unloadAsset(): Unit = {
    shaderImpl.unload()
    shaderImpl = null
  }
}
