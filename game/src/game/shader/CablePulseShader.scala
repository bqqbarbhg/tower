package game.shader

import render._
import asset._
import game.options.Options
import gfx.Shader

object CablePulseShader extends ShaderAsset("shader/mesh/effect/cable_pulse") {

  uniform(GlobalSceneUniform)
  uniform(CablePulseUniform)

  val MaskSampler = {
    import render.Sampler.Wrap._
    import render.Sampler.Filter._
    new Sampler(Clamp, Repeat, Linear, Linear, Linear, 0, "CablePulseShader.MaskSampler")
  }

  override object Textures extends SamplerBlock {
    val Mask = sampler2D("Mask", MaskSampler)
    val Texture = sampler2D("Texture", Sampler.RepeatTrilinear)
  }

}

