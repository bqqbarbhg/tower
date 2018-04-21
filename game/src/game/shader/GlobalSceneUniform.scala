package game.shader

import render._

object GlobalSceneUniform extends UniformBlock("GlobalSceneUniform") {

  val ViewProjection = mat4("ViewProjection")
  val ShadowViewProjection = mat4("ShadowViewProjection")
  val ViewPosition = vec4("ViewPosition")
  val ShadowSize = vec4("ShadowSize")
  val LightDirection = vec4("LightDirection")
  val LightIntensity = vec4("LightIntensity")

}

