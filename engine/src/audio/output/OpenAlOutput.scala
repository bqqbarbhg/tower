package audio.output

import core._
import java.nio.ShortBuffer

import org.lwjgl.openal.AL
import org.lwjgl.openal.ALC
import org.lwjgl.openal.AL10._
import org.lwjgl.openal.AL11._
import org.lwjgl.openal.ALC10._
import OpenAlOutput._
import org.lwjgl.system.MemoryUtil

import scala.collection.mutable

object OpenAlOutput {

  type ALDevice = Long
  type ALContext = Long
  type ALBuffer = Int
  type ALSource = Int

}

/**
  * Audio output using the OpenAL API.
  *
  * @param sampleRate Number of samples to play back per second
  * @param debug If set to true assert that the OpenAL state is ok
  */
class OpenAlOutput(val sampleRate: Int, val debug: Boolean) extends AudioOutput {

  private val FramesPerChunk = math.max(sampleRate / 100, 128)
  private val SamplesPerChunk = FramesPerChunk * 2

  private var chunkBuffer: ShortBuffer = null
  private var chunkBufferNumFrames = 0

  private var device: ALDevice = 0
  private var context: ALContext = 0
  private var source: ALSource = 0
  private var bufferPool = mutable.ArrayStack[ALBuffer]()
  private var numBuffersQueued = 0
  private var numFramesPlayed = 0

  private var shouldPlay = false

  /** Check for AL errors */
  private def check(): Unit = {
    if (!debug) return
    val error = alGetError()
    assert(error == 0, s"OpenAL error: $error")
  }

  /** Unqueue all the buffers that are free */
  private def unqueueBuffers(): Unit = withStack {
    if (numBuffersQueued > 0) {
      check()

      var numFree = alGetSourcei(source, AL_BUFFERS_PROCESSED)
      val bufs = alloca(numFree * 4).asIntBuffer
      alSourceUnqueueBuffers(source, bufs)
      check()

      for (i <- 0 until numFree) {
        bufferPool.push(bufs.get(i))
      }
      numBuffersQueued -= numFree
      numFramesPlayed += FramesPerChunk * numFree
    }
  }

  /** Write the contents of `chunkBuffer` to OpenAL */
  private def flushChunkBuffer(): Unit = {
    assert(chunkBufferNumFrames == FramesPerChunk)
    check()

    // Create or re-use a buffer
    val buffer = if (bufferPool.nonEmpty) {
      bufferPool.pop()
    } else {
      alGenBuffers()
    }
    check()

    // Copy the data to the buffer
    alBufferData(buffer, AL_FORMAT_STEREO16, chunkBuffer, sampleRate)
    check()

    // Queue the buffer to the source
    alSourceQueueBuffers(source, buffer)
    check()

    // Make sure the source is playing
    val state = alGetSourcei(source, AL_SOURCE_STATE)
    check()
    if (state != AL_PLAYING && shouldPlay) {
      if (debug) {
        println("OpenAL warning: Source ran out and will be restarted.")
      }

      alSourcePlay(source)
      check()
    }

    // Reset the size of the chunk buffer
    chunkBufferNumFrames = 0
    numBuffersQueued += 1
  }

  /** Initialize AL and allocate memory */
  override def open(): Unit = {
    chunkBuffer = MemoryUtil.memAllocShort(SamplesPerChunk)

    val deviceName = alcGetString(0, ALC_DEFAULT_DEVICE_SPECIFIER)
    device = alcOpenDevice(deviceName)
    context = alcCreateContext(device, Array(0))
    if (debug) {
      val error = alcGetError(device)
      assert(error == 0, s"OpenAL context error: $error")
    }

    alcMakeContextCurrent(context)
    val alcCapabilities = ALC.createCapabilities(device)
    val alCapabilities = AL.createCapabilities(alcCapabilities)
    check()

    source = alGenSources()
    check()
  }

  /** Unqueue all buffers, close AL, and free memory */
  override def close(): Unit = {
    check()

    // Write the pending data in the chunk buffer (if not empty)
    if (chunkBufferNumFrames > 0) {
      assert(chunkBufferNumFrames < FramesPerChunk)
      for (frame <- chunkBufferNumFrames until FramesPerChunk) {
        val sample = frame * 2
        chunkBuffer.put(sample + 0, 0)
        chunkBuffer.put(sample + 1, 0)
      }
      chunkBufferNumFrames = FramesPerChunk
      flushChunkBuffer()
    }

    // Unqueue all the buffers from the source
    unqueueBuffers()
    while (numBuffersQueued > 0) {
      Thread.sleep(5)
      unqueueBuffers()
    }

    // Wait that the source stops playing
    var state = alGetSourcei(source, AL_SOURCE_STATE)
    check()
    while (state == AL_PLAYING) {
      Thread.sleep(5)
      state = alGetSourcei(source, AL_SOURCE_STATE)
      check()
    }

    // OpenAL seems to report the source to be stopped before it actually is,
    // which causes popping. Wait for 100ms before actually closing OpenAL.
    Thread.sleep(100)

    // All the buffers have been unqueued to the pool, so they can be freed
    alDeleteBuffers(bufferPool.toArray)
    check()

    // Close the source, context and device
    if (source != 0) {
      alSourceStop(source);    check()
      alDeleteSources(source); check()
    }

    // Detach the current context
    alcMakeContextCurrent(0)

    // Release the context and the device
    alcDestroyContext(context)
    if (debug) {
      val error = alcGetError(device)
      assert(error == 0, s"OpenAL context error: $error")
    }

    alcCloseDevice(device)

    MemoryUtil.memFree(chunkBuffer)
    chunkBuffer = null
  }

  /** Start the source and flag it to be restarted if it runs out */
  override def start(): Unit = {
    alSourcePlay(source)
    shouldPlay = true
  }

  /** Write data to buffers and queue them */
  override def write(data: Array[Float], numFrames: Int): Unit = {
    // Unqueue buffers to potentially re-use buffers
    unqueueBuffers()

    var numFramesWritten = 0
    while (numFramesWritten < numFrames) {
      val framesLeftSrc = numFrames - numFramesWritten
      val framesLeftDst = FramesPerChunk - chunkBufferNumFrames
      val framesToWrite = math.min(framesLeftSrc, framesLeftDst)
      val samplesToWrite = framesToWrite * 2

      var i = 0
      var srcBase = numFramesWritten * 2
      var dstBase = chunkBufferNumFrames * 2
      while (i < samplesToWrite) {
        val sample = data(srcBase + i)
        val sampleI = clamp((sample * 32767.0f).toInt, -32767, 32767)
        chunkBuffer.put(dstBase + i, sampleI.toShort)
        i += 1
      }

      chunkBufferNumFrames += framesToWrite
      numFramesWritten += framesToWrite

      if (chunkBufferNumFrames >= FramesPerChunk)
        flushChunkBuffer()
    }
  }

  override def playbackFrame: Long = {
    // Unqueue buffers to update playback cursor
    unqueueBuffers()
    numFramesPlayed
  }

}
