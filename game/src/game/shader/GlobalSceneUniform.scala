package game.shader

import render._

object GlobalSceneUniform extends UniformBlock("GlobalSceneUniform") {

  val ViewProjection = mat4("ViewProjection")

}

