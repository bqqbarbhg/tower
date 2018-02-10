package tower.engine.audio.platform

import java.nio.ByteBuffer

import org.lwjgl.BufferUtils
import org.lwjgl.openal.AL
import org.lwjgl.openal.ALC
import org.lwjgl.openal.AL10._
import org.lwjgl.openal.AL11._
import org.lwjgl.openal.ALC10._
import tower.engine.audio.AudioOutput

class OpenAlOutput(val sampleRate: Int) extends AudioOutput {
  private var device: Long = 0
  private var context: Long = 0
  private val samplesPerChunk = 1024
  private val chunkBuffer = BufferUtils.createShortBuffer(samplesPerChunk)

  private var ringBuffers = new Array[Int](4)
  private var numBuffersQueued = 0
  private var source: Int = 0

  private def check(): Unit = {
    val error = alGetError()
    assert(error == 0, s"OpenAL error: $error")
  }

  override def initialize(): Unit = this.synchronized {
    val deviceName = alcGetString(0, ALC_DEFAULT_DEVICE_SPECIFIER)
    device = alcOpenDevice(deviceName)
    context = alcCreateContext(device, Array(0))
    alcMakeContextCurrent(context)

    val alcCapabilities = ALC.createCapabilities(device)
    val alCapabilities = AL.createCapabilities(alcCapabilities)

    if (!(alCapabilities.OpenAL10 && alCapabilities.OpenAL11)) {
      throw new RuntimeException("OpenAL 1.1 required for source playback position query")
    }


    check()
    alGenBuffers(ringBuffers)
    check()
    source = alGenSources()
    check()
  }

  override def write(sampleData: Array[Float], numFrames: Int): Unit = this.synchronized {
    check()

    val numSamples = numFrames * 2
    var position = 0
    while (position < numSamples) {
      val toWrite = math.min(numSamples - position, samplesPerChunk)
      var offset = 0
      while (offset < toWrite) {
        val sample = sampleData(position + offset)
        val sampleI = (sample * 32767.0f * 0.8f).toInt
        chunkBuffer.put(offset, sampleI.toShort)
        offset += 1
      }

      val buf = if (numBuffersQueued < ringBuffers.length) {
        val index = numBuffersQueued
        numBuffersQueued += 1
        ringBuffers(index)
      } else {
        var numFree = alGetSourcei(source, AL_BUFFERS_PROCESSED)
        while (numFree == 0) {
          Thread.sleep(1)
          numFree = alGetSourcei(source, AL_BUFFERS_PROCESSED)
        }
        val buffer = alSourceUnqueueBuffers(source)
        check()
        buffer
      }

      val src = chunkBuffer.duplicate
      src.limit(toWrite)
      alBufferData(buf, AL_FORMAT_STEREO16, src, sampleRate)
      check()
      alSourceQueueBuffers(source, buf)
      check()

      val state = alGetSourcei(source, AL_SOURCE_STATE)
      check()

      if (state != AL_PLAYING) {
        alSourcePlay(source)
        check()
      }

      position += toWrite
    }
  }

  override def currentFrame: Long = this.synchronized {
    0
  }

  override def close(): Unit = {
    var state = alGetSourcei(source, AL_SOURCE_STATE)
    check()
    while (state == AL_PLAYING) {
      Thread.sleep(1)
      state = alGetSourcei(source, AL_SOURCE_STATE)
      check()
    }
    Thread.sleep(100)

    alSourceStop(source)
    alDeleteSources(source)
    alDeleteBuffers(ringBuffers)
    if (context != 0)
      alcDestroyContext(context)
    if (device != 0)
      alcCloseDevice(device)
  }
}

