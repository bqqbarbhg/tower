package game.system.audio

import asset.SoundAsset
import core._
import audio._
import audio.effect._
import audio.output._
import game.options.Options

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import AudioSystem._

object AudioSystem {
  class SoundRef(private[system] val mixer: Mixer, val instance: SoundInstance) {
    private[system] var stopFrame: Long = -1L

    def stop(): Unit = {
      instance.volume = 0.0
      stopFrame = instance.framesPlayed
    }
  }

  object AudioChannel extends Enumeration {
    type AudioChannel = Value
    val Sfx, Ui, Music = Value
  }

  val Sfx = AudioChannel.Sfx
  val Ui = AudioChannel.Ui
  val Music = AudioChannel.Music
}

sealed trait AudioSystem {

  /** Request the audio system to enter a high-latency state.
    * Useful when garbage collecting etc. */
  def acquireHighLatency(): Unit

  /** End high-latency request */
  def releaseHighLatency(): Unit

  /** Sync state changes to the audio thread */
  def update(): Unit

  /** Play a sound */
  def play(soundAsset: SoundAsset, channel: AudioChannel.AudioChannel, volume: Double = 1.0, pan: Double = 0.0, pitch: Double = 1.0, startFrame: Double = 0.0): SoundRef

  /** Wait until the audio thread is finished */
  def joinAudioThread(): Unit

  /** Begin stopping the system */
  def unload(): Unit

}

final class AudioSystemImpl extends AudioSystem {
  val Opt = Options.current.audio

  val SamplesPerFrame = Opt.sampleRate / 60.0

  val systemAudioOutput: AudioOutput = {
    val primaryOutput = Opt.audioOutput match {
      case "OpenAL" => new OpenAlOutput(Opt.sampleRate, Opt.debug)
    }

    if (Opt.debugFilename != "") {
      val debugOutput = new FileAudioOutput(Opt.debugFilename)
      new MultiAudioOutput(Seq(primaryOutput, debugOutput))
    } else {
      primaryOutput
    }
  }

  val mixerSfx = new Mixer()
  val mixerUi = new Mixer()
  val mixerMusic = new Mixer()
  val mixerFinal = new Mixer()
  val limiter = new Limiter(mixerFinal)

  mixerFinal.add(mixerSfx)
  mixerFinal.add(mixerUi)
  mixerFinal.add(mixerMusic)

  val newInstances = new ArrayBuffer[SoundRef]()
  val freeInstances = new mutable.ArrayStack[Int]()
  var activeInstances = new ArrayBuffer[SoundRef]()

  @volatile var sharedCloseAudio: Boolean = false
  @volatile var sharedAudioLatencySec: Double = Opt.latency

  var highLatencyCounter = 0

  override def acquireHighLatency(): Unit = {
    highLatencyCounter += 1
    sharedAudioLatencySec = 0.3
  }

  override def releaseHighLatency(): Unit = {
    highLatencyCounter -= 1
    if (highLatencyCounter == 0) {
      sharedAudioLatencySec = Opt.latency
    }
  }

  override def update(): Unit = this.synchronized {
    mixerSfx.volume = Opt.volumeSfx.toFloat
    mixerUi.volume = Opt.volumeUi.toFloat
    mixerMusic.volume = Opt.volumeMusic.toFloat
    mixerFinal.volume = Opt.volumeMaster.toFloat

    for (inst <- newInstances) {
      val ix = if (freeInstances.nonEmpty) {
        freeInstances.pop()
      } else {
        activeInstances += null
        activeInstances.length - 1
      }

      inst.mixer.add(inst.instance)
      activeInstances(ix) = inst
    }
    newInstances.clear()

    var ix = 0
    while (ix < activeInstances.length) {
      val inst = activeInstances(ix)
      if (inst != null) {
        inst.instance.copyParameters()

        if (!inst.instance.sound.loaded) {
          val asset = SoundAsset(inst.instance.sound.filename)
          if (asset.isReferenced) {
            inst.instance.reload(asset.get)
          } else {
            inst.stop()
          }
        }

        if (inst.stopFrame >= 0L || inst.instance.ended) {
          val delta = inst.instance.framesPlayed - inst.stopFrame
          if (delta >= SamplesPerFrame * 2 || inst.instance.ended) {
            inst.instance.close()
            inst.mixer.remove(inst.instance)
            activeInstances(ix) = null
            freeInstances.push(ix)
          }
        }
      }
      ix += 1
    }

    // If at least quarter of the instances are `null` and compact the pool
    if (activeInstances.length >= 16 && freeInstances.length >= activeInstances.length / 4) {
      activeInstances = activeInstances.filter(_ != null)
      freeInstances.clear()
    }
  }

  def renderAudio(data: Array[Float], numFrames: Int): Unit = this.synchronized {
    limiter.advance(data, 0, numFrames, Opt.sampleRate)
  }

  val audioThread = new Thread() {
    override def run(): Unit = {
      val audioOutput = systemAudioOutput

      // Disable denormals for audio performance
      platform.Intrinsic.disableDenormals()
      core.StackAllocator.createCurrentThread(2 * 1024 * 1024)
      audioOutput.open()

      var framesWritten: Long = 0

      val BeginSilenceLength = 1500
      val EndSilenceLength = 1500
      val FadeInLength = 500
      val FadeOutLength = 500

      val ChunkSize = 1000
      val chunk = new Array[Float](ChunkSize * 2)

      {
        java.util.Arrays.fill(chunk, 0.0f)
        var silenceWritten = 0
        while (silenceWritten < BeginSilenceLength) {
          val toWrite = math.min(BeginSilenceLength - silenceWritten, ChunkSize)
          framesWritten += toWrite
          audioOutput.write(chunk, toWrite)
          silenceWritten += toWrite
        }
      }

      renderAudio(chunk, FadeInLength)
      framesWritten += FadeInLength
      for (i <- 0 until FadeInLength) {
        val s = i * 2
        val fade = i.toFloat / FadeInLength.toFloat
        chunk(s + 0) *= fade
        chunk(s + 1) *= fade
      }
      audioOutput.write(chunk, FadeInLength)

      audioOutput.start()

      while (!sharedCloseAudio) {
        val audioLatency = (sharedAudioLatencySec * Opt.sampleRate).toInt

        val playPos = audioOutput.playbackFrame
        val lead = framesWritten - playPos

        val maxWrite = math.max(audioLatency - lead, 0)
        if (maxWrite > 0) {
          val toWrite = math.min(maxWrite, ChunkSize).toInt
          renderAudio(chunk, toWrite)
          framesWritten += toWrite
          audioOutput.write(chunk, toWrite)
        } else {
          Thread.sleep(5)
        }
      }

      renderAudio(chunk, FadeOutLength)
      framesWritten += FadeOutLength
      for (i <- 0 until FadeOutLength) {
        val s = i * 2
        val fade = 1.0f - i.toFloat / FadeOutLength.toFloat
        chunk(s + 0) *= fade
        chunk(s + 1) *= fade
      }
      audioOutput.write(chunk, FadeOutLength)

      {
        java.util.Arrays.fill(chunk, 0.0f)
        var silenceWritten = 0
        while (silenceWritten < EndSilenceLength) {
          val toWrite = math.min(EndSilenceLength - silenceWritten, ChunkSize)
          framesWritten += toWrite
          audioOutput.write(chunk, toWrite)
          silenceWritten += toWrite
        }
      }

      audioOutput.close()
    }
  }

  audioThread.setName("Audio Thread")
  audioThread.setPriority(Thread.MAX_PRIORITY)
  audioThread.start()

  override def unload(): Unit = {
    sharedCloseAudio = true
  }

  override def joinAudioThread(): Unit = {
    audioThread.join()

    for (inst <- activeInstances) {
      if (inst != null) {
        inst.instance.close()
      }
    }
  }

  override def play(soundAsset: SoundAsset, channel: AudioChannel.AudioChannel, volume: Double = 1.0, pan: Double = 0.0, pitch: Double = 1.0, startFrame: Double = 0.0): SoundRef = {
    val mixer = channel match {
      case AudioChannel.Sfx => mixerSfx
      case AudioChannel.Ui => mixerUi
      case AudioChannel.Music => mixerMusic
    }

    val sound = soundAsset.get
    val inst = SoundInstance.make(sound).orElse(SoundInstance.make(NullSound)).get
    inst.volume = volume
    inst.pan = pan
    inst.pitch = pitch
    if (startFrame != 0.0)
      inst.forceTime(startFrame)

    val ref = new SoundRef(mixer, inst)
    newInstances += ref
    ref
  }

}


