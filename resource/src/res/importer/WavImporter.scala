package res.importer

import org.lwjgl.system.MemoryUtil

import res.intermediate._
import core._
import util.BufferUtils._

object WavImporter extends Importer {
  override def importType: ImportFileType = ImportFilePcm

  def importAsset(asset: AssetFile): Iterable[Resource] = {
    val buffer = MemoryUtil.memAlloc(64 * 1024 * 1024)
    buffer.readFromFile(asset.file)
    buffer.finish()

    buffer.verifyMagic("RIFF")
    val totalSize = buffer.getInt()
    buffer.verifyMagic("WAVE")
    buffer.verifyMagic("fmt ")
    val fmtSize = buffer.getInt()
    val audioFormat = buffer.getShort().toInt
    assert(fmtSize == 16)
    assert(audioFormat == 1) // 1 == PCM

    val pcmSound = new PcmSound()
    val numChannels = buffer.getShort().toInt
    pcmSound.sampleRate = buffer.getInt()

    val byteRate = buffer.getInt()
    val blockAlign = buffer.getShort().toInt
    val bitsPerSample = buffer.getShort().toInt

    buffer.verifyMagic("data")
    val numBytes = buffer.getInt()
    pcmSound.numSamples = numBytes / blockAlign
    pcmSound.data = Array.fill(numChannels)(new Array[Double](pcmSound.numSamples))

    val nc = pcmSound.numChannels
    val data = pcmSound.data
    val numData = pcmSound.numChannels * pcmSound.numSamples
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

    MemoryUtil.memFree(buffer)
    Some(pcmSound)
  }
}
