package render

import collection.mutable.ArrayBuffer

import SamplerBlock._

object SamplerBlock {

  object NoSamplers extends SamplerBlock {
  }

  sealed abstract class USampler(val name: String, val index: Int, val sampler: Sampler)
  class USampler2D(name: String, index: Int, sampler: Sampler) extends USampler(name, index, sampler)
  class USampler2DArray(name: String, index: Int, sampler: Sampler) extends USampler(name, index, sampler)

  private var maxSamplers: Int = 0
  def maxSamplersInBlock: Int = maxSamplers
}

class SamplerBlock {
  val samplers = new ArrayBuffer[USampler]()

  private def push[T <: USampler](sampler: T): T = {
    samplers += sampler

    if (samplers.length > SamplerBlock.maxSamplers) {
      SamplerBlock.synchronized {
        SamplerBlock.maxSamplers = math.max(SamplerBlock.maxSamplers, samplers.length)
      }
    }

    sampler
  }

  def sampler2D(name: String, sampler: Sampler): USampler2D = push(new USampler2D(name, samplers.length, sampler))
  def sampler2DArray(name: String, sampler: Sampler): USampler2DArray = push(new USampler2DArray(name, samplers.length, sampler))
}

