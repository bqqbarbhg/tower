package game.shader

import render._
import asset._
import game.options.Options
import gfx.Shader

object PlaceMeshShader extends ShaderAsset("shader/mesh/effect/place_mesh") {

  uniform(GlobalSceneUniform)

  uniform(VertexUniform)
  object VertexUniform extends UniformBlock("VertexUniform") {
    val World = mat4x3("World")
    val UvBounds = vec4("UvBounds")
  }

  uniform(PixelUniform)
  object PixelUniform extends UniformBlock("PixelUniform") {
    val UvOffset = vec4("UvOffset")
    val Color = vec4("Color")
  }

  override object Textures extends SamplerBlock {
    val Placemask = sampler2D("Placemask", Sampler.RepeatAnisotropic)
    val Noise = sampler2D("Noise", Sampler.RepeatAnisotropic)
  }

}

