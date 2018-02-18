package res.intermediate

import java.nio.ByteBuffer

class PcmSound extends Resource {
  var sampleRate: Int = 0
  var numSamples: Int = 0
  var data: Array[Array[Double]] = Array()
  def numChannels = data.length

  override def unload(): Unit = {
    data = Array[Array[Double]]()
  }
}
