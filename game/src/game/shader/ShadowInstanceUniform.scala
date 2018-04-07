package game.shader

import render._

object ShadowInstanceUniform extends UniformBlock("ShadowInstanceUniform") {
  val MaxInstancesPerDraw = 16

  val World = mat4x3("World", MaxInstancesPerDraw)
}

