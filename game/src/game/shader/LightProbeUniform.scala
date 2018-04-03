package game.shader

import render._
import java.nio.ByteBuffer

import game.lighting.LightProbe

object LightProbeUniform extends UniformBlock("LightProbeUniform") {
  val MaxProbes = 16

  val LightProbes = vec4("LightProbes", MaxProbes * LightProbe.SizeInVec4)

  def write(u: ByteBuffer, probes: Array[LightProbe]): Unit = {
    var ix = 0
    var base = LightProbeUniform.LightProbes.offsetInBytes
    val stride = LightProbeUniform.LightProbes.arrayStrideInBytes
    val baseStride = LightProbe.SizeInVec4 * stride
    while (ix < probes.length) {
      probes(ix).writeToUniform(u, base, stride)
      ix += 1
      base += baseStride
    }
  }
}

