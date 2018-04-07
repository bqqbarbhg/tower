package game.shader

import render._

object SkinnedShadowUniform extends UniformBlock("SkinnedShadowUniform") {
  val MaxBones = 24

  val Bones = mat4x3("Bones", MaxBones)
}

