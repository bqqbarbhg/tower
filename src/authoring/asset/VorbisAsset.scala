package tower.authoring.asset

import java.nio.file.{Files, Paths}

import org.lwjgl.stb.STBVorbis._
import org.lwjgl.stb.STBVorbisInfo
import tower.authoring.Asset
import tower.authoring.Resource
import tower.authoring.resource._

class VorbisAsset(filename: String, baseName: String) extends Asset(filename, baseName) {

  val error = Array(0)
  val vorbis = stb_vorbis_open_filename(filename, error, null)

  val resource = new AudioResource(baseName + ".s2au")

  val info = STBVorbisInfo.calloc()
  stb_vorbis_get_info(vorbis, info)

  resource.format = AudioResource.Format.OggVorbis
  resource.numChannels = info.channels
  resource.sampleRate = info.sample_rate
  resource.numSamples = stb_vorbis_stream_length_in_samples(vorbis)

  resource.data = Files.readAllBytes(Paths.get(filename))

  stb_vorbis_close(vorbis)
  info.free()

  def resources: Seq[Resource] = Array(resource)
}
