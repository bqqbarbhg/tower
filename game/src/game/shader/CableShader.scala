package game.shader

import render._
import asset._
import game.options.Options
import gfx.Shader

object CableShader extends ShaderAsset("shader/mesh/cable") {

  uniform(GlobalSceneUniform)
  uniform(LightProbeUniform)

  override object Textures extends SamplerBlock {
    val ShadowMap = sampler2D("ShadowMap", Sampler.ClampBilinearNoMip)
  }

  override object Defines extends Shader.Defines {
    both("MaxLightProbes", LightProbeUniform.MaxProbes)
    both("ShaderQuality", Options.current.graphics.quality.shaderQuality)
  }

}


