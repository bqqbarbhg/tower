package tower.authoring.asset

import java.io.FileInputStream
import java.nio.file.{Files, Paths}
import java.nio.ByteBuffer

import tower.authoring.Asset
import tower.authoring.Resource
import tower.authoring.resource._
import tower.util.Serialization.ByteBufferExtension
import tower.util.SharedByteBuffer

class WavAsset(filename: String, baseName: String) extends Asset(filename, baseName) {

  val resource = new PcmResource(baseName + ".s2au")

  {
    val buffer = SharedByteBuffer.acquire()
    buffer.readFrom(new FileInputStream(filename))

    buffer.verifyMagic("RIFF")
    val totalSize = buffer.getInt()
    buffer.verifyMagic("WAVE")
    buffer.verifyMagic("fmt ")
    val fmtSize = buffer.getInt()
    val audioFormat = buffer.getShort().toInt
    assert(fmtSize == 16)
    assert(audioFormat == 1) // 1 == PCM

    resource.numChannels = buffer.getShort().toInt
    resource.sampleRate = buffer.getInt()

    val byteRate = buffer.getInt()
    val blockAlign = buffer.getShort().toInt
    val bitsPerSample = buffer.getShort().toInt

    buffer.verifyMagic("data")
    val numBytes = buffer.getInt()
    resource.numSamples = numBytes / blockAlign
    resource.data = Array.fill(resource.numChannels)(new Array[Double](resource.numSamples))

    println(baseName)
    val nc = resource.numChannels
    val data = resource.data
    val numData = resource.numChannels * resource.numSamples
    var i = 0
    bitsPerSample match {
      case 8 =>
        while (i < numData) {
          data(i % nc)(i / nc) = buffer.get().toDouble / (1L << 7).toDouble
          i += 1
        }
      case 16 =>
        while (i < numData) {
          data(i % nc)(i / nc) = buffer.getShort().toDouble / (1L << 15).toDouble
          i += 1
        }
      case 24 =>
        while (i < numData) {
          val low = buffer.get()
          val high = buffer.getShort()
          val v = low | (high << 8)
          data(i % nc)(i / nc) = v.toDouble / (1L << 23).toDouble
          i += 1
        }
      case 32 =>
        while (i < numData) {
          data(i % nc)(i / nc) = buffer.getInt().toDouble / (1L << 31).toDouble
          i += 1
        }
    }

    SharedByteBuffer.release(buffer)
  }

  def resources: Seq[Resource] = Array(resource)
}
