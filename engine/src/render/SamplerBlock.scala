package render

import collection.mutable.ArrayBuffer

import SamplerBlock._

object SamplerBlock {

  class USampler2D(val name: String, val index: Int, val sampler: Sampler)

  private var maxSamplers: Int = 0
  def maxSamplersInBlock: Int = maxSamplers
}

class SamplerBlock {
  val samplers = new ArrayBuffer[USampler2D]()

  private def push(sampler: USampler2D): USampler2D = {
    samplers += sampler

    if (samplers.length > SamplerBlock.maxSamplers) {
      SamplerBlock.synchronized {
        SamplerBlock.maxSamplers = math.max(SamplerBlock.maxSamplers, samplers.length)
      }
    }

    sampler
  }

  def sampler2D(name: String, sampler: Sampler): USampler2D = push(new USampler2D(name, samplers.length, sampler))
}

