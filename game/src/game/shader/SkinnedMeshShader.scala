package game.shader

import render._
import asset._
import game.lighting.LightProbe
import gfx.Shader

object SkinnedMeshShader extends ShaderAsset("shader/mesh/skinned_mesh") {

  uniform(GlobalSceneUniform)
  uniform(SkinnedModelUniform)

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


  override object Permutations extends Shader.Permutations {
    val BonesPerVertex = vert("BonesPerVertex", Seq(1, 4))
  }

  override object Defines extends Shader.Defines {
    both("MaxBones", SkinnedModelUniform.MaxBones)
  }

  def getBonePermutation(maxBones: Int): Int = {
    if (maxBones == 1) 1
    else 4
  }

}

