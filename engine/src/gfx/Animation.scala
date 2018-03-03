package gfx

import java.nio.ByteBuffer
import org.lwjgl.system.MemoryUtil

import core._
import util.BufferUtils._
import Animation._
import io.content.Package

object Animation {

  /** State needed by the animation playback */
  type OpaqueState = Array[Int]

  object Timeline {
    /** Requires 3 integers per timeline: previous index of position, rotation, size */
    val StateSize = 3
  }

  /**
    * Represents a timeline for a single bone in the animation.
    *
    * The actual storage is optimized in the `data` array of the main `Animation` object.
    *
    * Data storage:
    * [rot time]  T
    * [rot value] X Y Z W
    * [pos time]  T
    * [pos value] X Y Z
    * [siz time]  T
    * [siz value] X Y Z
    *
    * @param bone Name of the bone to apply the transformation
    * @param baseOffset Base index in the data array of the timeline's data
    * @param numRotKeys Number of keys for rotation
    * @param numPosKeys Number of keys for position
    * @param numSizKeys Number of keys for size
    */
  class Timeline(val anim: Animation, val bone: Identifier, val baseOffset: Int, val numRotKeys: Int, val numPosKeys: Int, val numSizKeys: Int, val flags: Int, val index: Int) {

    // Offsets into the data buffer
    val rotTimeIndex: Int = baseOffset
    val posTimeIndex: Int = baseOffset + numRotKeys * 5
    val sizTimeIndex: Int = baseOffset + numRotKeys * 5 + numPosKeys * 4
    val rotValueIndex: Int = rotTimeIndex + numRotKeys
    val posValueIndex: Int = posTimeIndex + numPosKeys
    val sizValueIndex: Int = sizTimeIndex + numSizKeys

    private var constRot: Quaternion = null
    private var constPos: Vector3 = null
    private var constSiz: Vector3 = null

    /**
      * This will be called when the data for the animation has been loaded.
      */
    def animationDataLoaded(): Unit = {
      if ((flags & 0x01) != 0) {
        val (b, d) = (rotValueIndex, anim.data)
        constRot = Quaternion(d(b + 0), d(b + 1), d(b + 2), d(b + 3))
      }

      if ((flags & 0x02) != 0) {
        val (b, d) = (posValueIndex, anim.data)
        constPos = Vector3(d(b + 0), d(b + 1), d(b + 2))
      }

      if ((flags & 0x04) != 0) {
        val (b, d) = (sizValueIndex, anim.data)
        constSiz = Vector3(d(b + 0), d(b + 1), d(b + 2))
      }
    }

    /**
      * Find a keyframe which time is lower than or equal to `time`.
      * Guaranteed to return an index [0, numFrames[
      */
    private def findKeyframe(time: Float, dataOffset: Int, numFrames: Int, state: OpaqueState, stateOffset: Int): Int = {
      val hint = state(stateOffset)
      val data = anim.data

      // Try to guess the frame based on previous state
      if (hint >= 0 && hint + 1 < numFrames && data(dataOffset + hint) <= time) {
        // Most likely: Still the same frame
        if (data(dataOffset + hint + 1) > time)
          return hint

        // Maybe the next one?
        if (hint + 2 < numFrames && data(dataOffset + hint + 2) > time) {
          state(stateOffset) = hint + 1
          return hint + 1
        }
      }

      // Search for the upper bound of the time
      var first = 0
      var count = numFrames
      while (count > 0) {
        val step = count >> 1
        val it = first + step
        if (time >= data(dataOffset + it)) {
          first = it + 1
          count -= step + 1
        } else {
          count = step
        }
      }

      // The upper bound is the _next_ frame, adjust it if possible.
      // In theory `first` should never be zero since upper bound returns the
      // first element _greater_ than the time, but I don't trust floats to play nice.
      if (first > 0) first -= 1
      if (first + 1 >= numFrames) first = numFrames - 2
      state(stateOffset) = first
      first
    }

    /**
      * Evaluate the timeline at a specific time.
      *
      * @param time Time in seconds to evaluate into the animation.
      * @param state State retrieved from `animation.createState()`, every logical
      *              animation instance should have their own state.
      * @return Transform of the node at `time`.
      */
    def evaluate(time: Double, state: OpaqueState): AffineTransform = {
      val stateOffset = index * Timeline.StateSize
      val timeF = clamp(time.toFloat, 0.0f, anim.duration.toFloat)
      val data = anim.data

      val rotation = if (constRot != null) constRot else {
        val frame = findKeyframe(timeF, rotTimeIndex, numRotKeys, state, stateOffset + 0)
        val nextFrame = frame + 1
        val begin = data(rotTimeIndex + frame)
        val length = data(rotTimeIndex + nextFrame) - begin
        val alpha = (timeF - begin) / length
        val beta = 1.0f - alpha
        val ib = rotValueIndex + frame * 4
        val ia = rotValueIndex + nextFrame * 4
        val x = data(ia + 0) * alpha + data(ib + 0) * beta
        val y = data(ia + 1) * alpha + data(ib + 1) * beta
        val z = data(ia + 2) * alpha + data(ib + 2) * beta
        val w = data(ia + 3) * alpha + data(ib + 3) * beta
        Quaternion(x, y, z, w)
      }

      val position = if (constPos != null) constPos else {
        val frame = findKeyframe(timeF, posTimeIndex, numPosKeys, state, stateOffset + 1)
        val nextFrame = frame + 1
        val begin = data(posTimeIndex + frame)
        val length = data(posTimeIndex + nextFrame) - begin
        val alpha = (timeF - begin) / length
        val beta = 1.0f - alpha
        val ib = posValueIndex + frame * 3
        val ia = posValueIndex + nextFrame * 3
        val x = data(ia + 0) * alpha + data(ib + 0) * beta
        val y = data(ia + 1) * alpha + data(ib + 1) * beta
        val z = data(ia + 2) * alpha + data(ib + 2) * beta
        Vector3(x, y, z)
      }

      val scale = if (constSiz != null) constSiz else {
        val frame = findKeyframe(timeF, sizTimeIndex, numSizKeys, state, stateOffset + 2)
        val nextFrame = frame + 1
        val begin = data(sizTimeIndex + frame)
        val length = data(sizTimeIndex + nextFrame) - begin
        val alpha = (timeF - begin) / length
        val beta = 1.0f - alpha
        val ib = sizValueIndex + frame * 3
        val ia = sizValueIndex + nextFrame * 3
        val x = data(ia + 0) * alpha + data(ib + 0) * beta
        val y = data(ia + 1) * alpha + data(ib + 1) * beta
        val z = data(ia + 2) * alpha + data(ib + 2) * beta
        Vector3(x, y, z)
      }

      AffineTransform(position, scale, rotation)
    }
  }

  def load(name: Identifier): Option[Animation] = {
    Package.get.get(name).map(file => {
      val animation = new Animation()

      val buffer = MemoryUtil.memAlloc(file.sizeInBytes.toInt)
      val stream = file.read()
      buffer.readFrom(stream)
      buffer.finish()
      animation.load(buffer)
      stream.close()
      MemoryUtil.memFree(buffer)

      animation
    })
  }
}

class Animation {

  // All keyframe time/value data is stored in this buffer!
  var duration: Double = 0.0
  var data: Array[Float] = null
  var timelines: Array[Timeline] = null

  /** Create a new state object compatible with this animation */
  def createState(): OpaqueState = Array.fill(timelines.length * Timeline.StateSize)(0)

  /**
    * Load the animation from a .s2an -file.
    */
  def load(buffer: ByteBuffer): Unit = {

    // @Deserialize(s2an)

    // File header
    buffer.verifyMagic("s2an")
    val MaxVersion = 2
    val version = buffer.getVersion(MaxVersion)

    // Animation header
    this.duration = buffer.getDouble()

    // Timelines
    val numTimelines = buffer.getInt()
    timelines = new Array[Timeline](numTimelines)
    for (i <- 0 until numTimelines) {
      val bone = buffer.getIdentifier()
      val baseOffset = buffer.getInt()
      val numRotKeys = buffer.getInt()
      val numPosKeys = buffer.getInt()
      val numSizKeys = buffer.getInt()
      val flags = if (version >= 2) buffer.getInt() else 0x00

      timelines(i) = new Timeline(this, bone, baseOffset, numRotKeys, numPosKeys, numSizKeys, flags, i)
    }

    // Data
    val dataSize = buffer.getInt()
    data = new Array[Float](dataSize)
    buffer.asFloatBuffer.get(this.data)
    buffer.position(buffer.position + dataSize * 4)

    buffer.verifyMagic("E.an")

    for (timeline <- timelines) {
      timeline.animationDataLoaded()
    }
  }
}

