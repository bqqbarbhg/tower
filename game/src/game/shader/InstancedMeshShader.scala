package game.shader

import render._
import asset._
import game.options.Options
import gfx.Shader

object InstancedMeshShader extends ShaderAsset("shader/mesh/instanced_mesh") {

  uniform(GlobalSceneUniform)
  uniform(ModelInstanceUniform)
  uniform(LightProbeUniform)

  uniform(VertexUniform)
  object VertexUniform extends UniformBlock("VertexUniform") {
    val UvBounds = vec4("UvBounds")
  }

  override object Textures extends SamplerBlock {
    val MatAlbedo = sampler2D("MatAlbedo", Sampler.RepeatAnisotropic)
    val MatNormal = sampler2D("MatNormal", Sampler.RepeatAnisotropic)
    val MatRoughness = sampler2D("MatRoughness", Sampler.RepeatTrilinear)
    val MatMetallic = sampler2D("MatMetallic", Sampler.RepeatTrilinear)
    val MatAo = sampler2D("MatAo", Sampler.RepeatTrilinear)
  }

  override object Defines extends Shader.Defines {
    vert("MaxInstances", ModelInstanceUniform.MaxInstancesPerDraw)
    both("MaxLightProbes", LightProbeUniform.MaxProbes)
    both("ShaderQuality", Options.current.graphics.quality.shaderQuality)
  }

}

