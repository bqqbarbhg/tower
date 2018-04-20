package game.shader

import render._
import asset._
import game.options.Options
import gfx.Shader

object InstancedShadowShader extends ShaderAsset("shader/mesh/shadow/instanced_mesh_shadow") {

  uniform(GlobalSceneUniform)
  uniform(ShadowInstanceUniform)

  override object Defines extends Shader.Defines {
    vert("MaxInstances", ShadowInstanceUniform.MaxInstancesPerDraw)
  }

}
