package game.shader

import render._
import asset._
import gfx.Shader

object SimpleMeshShader extends ShaderAsset("shader/mesh/simple_mesh") {

  uniform(GlobalUniform)
  object GlobalUniform extends UniformBlock("GlobalUniform") {
    val ViewProjection = mat4("ViewProjection")
  }

  uniform(InstanceUniform)
  object InstanceUniform extends UniformBlock("InstanceUniform") {
    val World = mat4x3("World")
    val UvBounds = vec4("UvBounds")
  }

  override object Textures extends SamplerBlock {
    val Texture = sampler2D("Texture", Sampler.RepeatAnisotropic)
  }

}

