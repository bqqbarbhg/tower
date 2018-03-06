package audio

import core._

class SoundInstance(val sound: Sound) extends Input {

  var pitch: Double = 1.0
  var volume: Double = 1.0
  var pan: Double = 0.0

  private var snapParameters: Boolean = true
  private var targetPitch: Double = _
  private var targetVolumeLeft: Float = _
  private var targetVolumeRight: Float = _

  private var velocityPitch: Double = 0.0
  private var velocityVolumeLeft: Float = 0.0f
  private var velocityVolumeRight: Float = 0.0f

  private var interpPitch: Double = targetPitch
  private var interpVolumeLeft: Float = targetVolumeLeft
  private var interpVolumeRight: Float = targetVolumeRight

  private var timeInSourceFrames: Double = 0.0

  private val BufferMaxFrames = 64
  /** Local buffer of the sample data */
  private var buffer = new Array[Float](BufferMaxFrames * 2)
  /** Index of the first valid frame contained in `buffer` */
  private var bufferFirstFrame: Int = 0
  /** Index of the last valid frame contained in `buffer` */
  private var bufferLastFrame: Int = 0
  /** Number of frames contained in `buffer` */
  private var bufferNumFrames: Int = 0

  /** If true, the sound will loop between frames [loopBegin, loopEnd[ */
  private var loopEnabled: Boolean = false
  /** Inclusive first frame index of the looping area */
  private var loopBegin: Int = 0
  /** Exclusive last frame index of the looping area.
    * The frame `loopEnd` will be treated as if it would be `loopBegin` */
  private var loopEnd: Int = 0

  /** One past the last valid frame that will be read. Initially it will be
    *  at the end of the sound, but in looping case it can be the end of loop. */
  private var onePastLastFrameToRead = sound.lengthInFrames

  private val cursor = sound.sampleSource.open()

  private def followTarget(sampleRate: Int): Unit = {
    val factor = 60.0 / sampleRate.toDouble

    val velPitch = velocityPitch * factor
    val velVolLeft = velocityVolumeLeft * factor.toFloat
    val velVolRight = velocityVolumeRight * factor.toFloat
    interpPitch += clamp(targetPitch - interpPitch, -velPitch, velPitch)
    interpVolumeLeft += clamp(targetVolumeLeft - interpVolumeLeft, -velVolLeft, velVolLeft)
    interpVolumeRight += clamp(targetVolumeRight - interpVolumeRight, -velVolRight, velVolRight)
  }

  private def wrapTimeInLoop(): Unit = {
    val loopRange = loopEnd - loopBegin
    val loopBase = timeInSourceFrames.toInt - loopBegin
    val loopFraction = timeInSourceFrames - loopBase.toDouble
    val loopWrapped = loopBase % loopRange
    timeInSourceFrames = (loopBegin + loopWrapped).toDouble + loopFraction
  }

  private def invalidateBuffer(): Unit = {
    bufferFirstFrame = 0
    bufferLastFrame = 0
  }

  private def fillBuffer(): Unit = {
    val baseFrame = timeInSourceFrames.toInt
    if (baseFrame + 1 < bufferLastFrame + BufferMaxFrames - 1 && baseFrame >= bufferFirstFrame && bufferNumFrames > 0) {
      // Easy case: Next frame is located in the next buffer that will be read
      // Move the last frame to the first in case it's needed for interpolation
      val base = (bufferNumFrames - 1) * 2
      buffer(0) = buffer(base + 0)
      buffer(1) = buffer(base + 1)
      bufferFirstFrame = bufferLastFrame

      val toRead = math.min(onePastLastFrameToRead - (bufferLastFrame + 1), BufferMaxFrames - 1)
      bufferNumFrames = toRead + 1
      if (toRead > 0) {
        cursor.read(buffer, 1, toRead)
      }
    } else {
      // Hard case: Need to seek to set the cursor position
      bufferFirstFrame = baseFrame
      bufferNumFrames = math.min(onePastLastFrameToRead - bufferFirstFrame, BufferMaxFrames)
      if (bufferNumFrames > 0) {
        cursor.seek(baseFrame)
        cursor.read(buffer, 0, bufferNumFrames)
      }
    }

    // If looping is enabled and we have reached the end of the loop
    if (loopEnabled && bufferFirstFrame + bufferNumFrames >= loopEnd) {
      assert(bufferFirstFrame + bufferNumFrames == loopEnd, "Read past the end of the loop")
      if (bufferNumFrames < BufferMaxFrames) {
        cursor.seek(loopBegin)
        cursor.read(buffer, bufferNumFrames, 1)
        bufferNumFrames += 1
      }
    }

    bufferLastFrame = bufferFirstFrame + bufferNumFrames - 1
  }

  /**
    * Copy the parameters of the sound to the fields used by the sound playback.
    * Needs external synchronization: Not safe to call during `advance()`!
    */
  def copyParameters(): Unit = {
    val prevPitch = targetPitch
    val prevVolumeLeft = targetVolumeLeft
    val prevVolumeRight = targetVolumeRight

    targetPitch = pitch
    val angle = (pan + 1.0) * math.Pi / 4.0
    targetVolumeLeft = (volume * math.cos(angle)).toFloat
    targetVolumeRight = (volume * math.sin(angle)).toFloat

    if (snapParameters) {
      velocityPitch = 100.0
      velocityVolumeLeft = 100.0f
      velocityVolumeRight = 100.0f
      snapParameters = false
    } else {
      velocityPitch = targetPitch - interpPitch
      velocityVolumeLeft = math.abs(targetVolumeLeft - interpVolumeLeft)
      velocityVolumeRight = math.abs(targetVolumeRight - interpVolumeRight)
    }
  }

  /** Set the sound instance to loop between the full range */
  def setLoop(): Unit = setLoop(0, sound.lengthInFrames)

  /** Set the sound instance to loop between [begin, end[ */
  def setLoop(begin: Int, end: Int): Unit = {
    invalidateBuffer()
    loopEnabled = true
    loopBegin = begin
    loopEnd = end
    onePastLastFrameToRead = loopEnd
  }

  /** Disable looping the sound */
  def clearLoop(): Unit = {
    invalidateBuffer()
    loopEnabled = false
    onePastLastFrameToRead = sound.lengthInFrames
  }

  override def advance(dstData: Array[Float], offsetInFrames: Int, numFrames: Int, sampleRate: Int): Unit = {
    val timeAdvance = sound.sampleRate.toDouble / sampleRate.toDouble

    var dstIndex = offsetInFrames
    val dstEnd = dstIndex + numFrames
    while (dstIndex < dstEnd) {
      if (loopEnabled && timeInSourceFrames.toInt >= loopEnd) {
        wrapTimeInLoop()
      }

      val time = timeInSourceFrames
      val baseFrame = time.toInt

      if (baseFrame < bufferFirstFrame || baseFrame + 1 > bufferLastFrame) {
        fillBuffer()
      }

      val bufOffset = baseFrame - bufferFirstFrame
      val beta = (time - time.floor).toFloat
      val alpha = 1.0f - beta
      val base = bufOffset << 1
      val al = buffer(base + 0)
      val ar = buffer(base + 1)
      val bl = buffer(base + 2)
      val br = buffer(base + 3)
      val sl = al * alpha + bl * beta
      val sr = ar * alpha + br * beta

      val dstBase = dstIndex << 1
      dstData(dstBase + 0) = sl * interpVolumeLeft
      dstData(dstBase + 1) = sr * interpVolumeRight
      timeInSourceFrames += timeAdvance * interpPitch

      followTarget(sampleRate)
      dstIndex += 1
    }

  }

  def close(): Unit = {
    cursor.close()
  }
}
