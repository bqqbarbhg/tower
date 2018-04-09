package game.shader

import render._
import asset._
import game.options.Options
import gfx.Shader

object TonemapShader extends ShaderAsset("shader/post/tonemap") {

  override object Defines extends Shader.Defines {
    frag("MsaaSamples", Options.current.graphics.quality.antialias)
    frag("PerSampleTonemap", 1)
  }

  override object Textures extends SamplerBlock {
    val Backbuffer = sampler2D("Backbuffer", Sampler.ClampBilinearNoMip)
    val BackbufferMsaa = sampler2DMS("BackbufferMsaa")
    val ColorLookup = sampler2D("ColorLookup", Sampler.ClampBilinearNoMip)
  }

}


