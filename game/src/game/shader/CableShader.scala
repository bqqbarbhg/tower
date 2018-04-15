package game.shader

import render._
import asset._
import game.options.Options
import gfx.Shader

object CableShader extends ShaderAsset("shader/mesh/cable") {

  uniform(GlobalSceneUniform)
  uniform(LightProbeUniform)

}


