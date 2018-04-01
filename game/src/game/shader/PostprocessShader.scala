package game.shader

import render._
import asset._
import game.options.Options
import gfx.Shader

object PostprocessShader extends ShaderAsset("shader/postprocess") {

  override object Textures extends SamplerBlock {
    val Backbuffer = sampler2D("Backbuffer", Sampler.ClampBilinearNoMip)
  }

}


