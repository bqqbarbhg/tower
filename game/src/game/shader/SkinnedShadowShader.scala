package game.shader

import render._
import asset._
import game.lighting.LightProbe
import gfx.Shader

object SkinnedShadowShader extends ShaderAsset("shader/mesh/shadow/skinned_mesh_shadow") {

  uniform(GlobalSceneUniform)
  uniform(SkinnedShadowUniform)

  override object Permutations extends Shader.Permutations {
    val BonesPerVertex = vert("BonesPerVertex", Seq(1, 4))
  }

  override object Defines extends Shader.Defines {
    both("MaxBones", SkinnedShadowUniform.MaxBones)
  }

  def getBonePermutation(maxBones: Int): Int = {
    if (maxBones == 1) 1
    else 4
  }

}

