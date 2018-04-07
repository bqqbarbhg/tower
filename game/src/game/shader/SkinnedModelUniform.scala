package game.shader

import render._
import java.nio.ByteBuffer

import game.lighting.LightProbe

object SkinnedModelUniform extends UniformBlock("SkinnedModelUniform") {
  val MaxBones = 24

  val Bones = mat4x3("Bones", MaxBones)
  val LightProbe = vec4("LightProbe", game.lighting.LightProbe.SizeInVec4)

  def writeLightProbe(u: ByteBuffer, probe: LightProbe): Unit = {
    var base = SkinnedModelUniform.LightProbe.offsetInBytes
    val stride = SkinnedModelUniform.LightProbe.arrayStrideInBytes
    probe.writeToUniform(u, base, stride)
  }
}

