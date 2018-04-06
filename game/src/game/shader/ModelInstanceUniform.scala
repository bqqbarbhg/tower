package game.shader

import render._

object ModelInstanceUniform extends UniformBlock("ModelInstanceUniform") {
  val MaxInstancesPerDraw = 8

  val World = mat4x3("World", MaxInstancesPerDraw)
  val LightInfo = ivec4("LightInfo", MaxInstancesPerDraw)
}

