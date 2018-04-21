package game.shader

import render._
import asset._
import game.options.Options
import gfx.Shader

object GroundShader extends ShaderAsset("shader/mesh/ground") {

  uniform(GlobalSceneUniform)
  uniform(GroundPlateUniform)
  uniform(LightProbeUniform)

  override object Textures extends SamplerBlock {
    val Albedo = sampler2D("Albedo", Sampler.RepeatTrilinear)
    val ShadowMap = sampler2D("ShadowMap", Sampler.ClampBilinearNoMip)
  }

  override object Defines extends Shader.Defines {
    both("MaxLightProbes", LightProbeUniform.MaxProbes)
    both("ShaderQuality", Options.current.graphics.quality.shaderQuality)
  }

}

