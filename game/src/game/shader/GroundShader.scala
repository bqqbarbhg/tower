package game.shader

import render._
import asset._
import gfx.Shader

object GroundShader extends ShaderAsset("shader/mesh/ground") {

  uniform(GlobalSceneUniform)
  uniform(GroundPlateUniform)
  uniform(LightProbeUniform)

  override object Textures extends SamplerBlock {
    val Albedo = sampler2D("Albedo", Sampler.RepeatTrilinear)
  }

}

