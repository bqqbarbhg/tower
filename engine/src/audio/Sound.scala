package audio

import java.nio.{ByteBuffer, ByteOrder}
import java.util.concurrent.atomic.AtomicInteger

import org.lwjgl.system.MemoryUtil
import audio.source.SampleSource
import core._
import io.ContentFile
import util.BufferUtils._
import io.content.Package
import task.Task

object Sound {
  def load(name: Identifier): Option[Sound] = Some(deferredLoad(name).get)

  def deferredLoad(name: Identifier): Task[Sound] = {
    ContentFile.load(name, buffer => {
      val sound = new Sound(name)
      sound.load(buffer)
      sound
    })
  }
}

class Sound(val filename: Identifier) {
  var format: String = ""
  var numChannels: Int = 0
  var sampleRate: Int = 0
  var lengthInFrames: Int = 0
  var sampleSource: SampleSource = null
  @volatile var loaded: Boolean = true

  /** Number of references to this, starts at one due to being loaded. */
  protected var refCount: Int = 1

  def load(buffer: ByteBuffer): Unit = {

    // @Serialize(s2au)
    buffer.verifyMagic("s2au")
    val MaxVersion = 1
    val version = buffer.getVersion(MaxVersion)

    this.format = buffer.getMagic()
    this.numChannels = buffer.getInt()
    this.sampleRate = buffer.getInt()
    this.lengthInFrames = buffer.getInt()

    val dataSize = buffer.getInt()

    val src = buffer.duplicateEx
    src.limit(src.position + dataSize)
    buffer.position(buffer.position + dataSize)

    this.sampleSource = format match {
      case "OGGV" =>
        val data = MemoryUtil.memAlloc(dataSize)
        data.order(ByteOrder.LITTLE_ENDIAN)
        data.put(src)
        data.position(0)
        new source.VorbisSource(data)
      case "PCMW" =>
        val samples = new Array[Short](this.numChannels * this.lengthInFrames)
        src.asShortBuffer.get(samples)
        new source.PcmSource(samples, this.numChannels)
      case _ => throw new RuntimeException(s"Unexpected audio format $format")
    }

    buffer.verifyMagic("E.au")
  }

  def acquire(): Boolean = this.synchronized {
    if (refCount >= 1) {
      refCount += 1
      true
    } else {
      false
    }
  }

  def release(): Unit = this.synchronized {
    refCount -= 1
    if (refCount == 0)
      sampleSource.unload()
  }

  def unload(): Unit = {
    loaded = false
    release()
  }
}