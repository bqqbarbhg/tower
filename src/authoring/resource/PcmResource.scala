package tower.authoring.resource

import java.nio.ByteBuffer

class PcmResource(name: String) extends tower.authoring.Resource(name) {
  var sampleRate: Int = 0
  var numSamples: Int = 0
  var numChannels: Int = 0
  var data: Array[Array[Double]] = Array()
}
