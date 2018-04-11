package game.shader

import render._
import asset._
import gfx.Shader

object InstancedMeshShader extends ShaderAsset("shader/mesh/instanced_mesh") {

  uniform(GlobalSceneUniform)
  uniform(ModelInstanceUniform)

  uniform(VertexUniform)
  object VertexUniform extends UniformBlock("VertexUniform") {
    val UvBounds = vec4("UvBounds")
  }

  override object Textures extends SamplerBlock {
    val Albedo = sampler2D("Albedo", Sampler.RepeatAnisotropic)
  }

  override object Defines extends Shader.Defines {
    vert("MaxInstances", ModelInstanceUniform.MaxInstancesPerDraw)
    vert("MaxProbes", LightProbeUniform.MaxProbes)
  }

}

