package res.importer

import java.nio.file.{Files, Paths}

import org.lwjgl.stb.STBVorbis._
import org.lwjgl.stb.STBVorbisInfo

import res.intermediate._

object StbVorbisImporter extends Importer {
  override def importType: ImportFileType = ImportFileAudio

  def importAsset(asset: AssetFile): Iterable[Resource] = {
    val filename = asset.file.getCanonicalFile.getAbsolutePath

    val error = Array(0)
    val vorbis = stb_vorbis_open_filename(filename, error, null)

    val sound = new Sound()

    val info = STBVorbisInfo.calloc()
    stb_vorbis_get_info(vorbis, info)

    sound.format = Sound.Format.OggVorbis
    sound.numChannels = info.channels
    sound.sampleRate = info.sample_rate
    sound.numSamples = stb_vorbis_stream_length_in_samples(vorbis)

    sound.data = Files.readAllBytes(Paths.get(filename))

    stb_vorbis_close(vorbis)
    info.free()

    Some(sound)
  }

}
