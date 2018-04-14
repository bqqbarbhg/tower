package game.shader

import render._
import asset._
import gfx.Shader

object SkinnedMeshShader extends ShaderAsset("shader/mesh/skinned_mesh") {

  uniform(GlobalSceneUniform)
  uniform(SkinnedModelUniform)

  uniform(VertexUniform)
  object VertexUniform extends UniformBlock("VertexUniform") {
    val UvBounds = vec4("UvBounds")
  }

  override object Textures extends SamplerBlock {
    val Albedo = sampler2D("Albedo", Sampler.RepeatAnisotropic)
  }

  override object Defines extends Shader.Defines {
    vert("MaxBones", SkinnedModelUniform.MaxBones)
  }

}

