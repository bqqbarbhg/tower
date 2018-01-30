package tower.engine

import java.nio.ByteBuffer

import tower.util.Serialization.ByteBufferExtension
import Animation._
import tower.Identifier

object Animation {

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
  case class Timeline(bone: Identifier, baseOffset: Int, numRotKeys: Int, numPosKeys: Int, numSizKeys: Int) {

    // Offsets into the data buffer
    def rotTimeIndex: Int = baseOffset
    def posTimeIndex: Int = baseOffset + numRotKeys * 5
    def sizTimeIndex: Int = baseOffset + numRotKeys * 5 + numPosKeys * 4
    def rotValueIndex: Int = rotTimeIndex + numRotKeys
    def posValueIndex: Int = posTimeIndex + numPosKeys
    def sizValueIndex: Int = sizTimeIndex + numSizKeys

  }

}

class Animation {

  // All keyframe time/value data is stored in this buffer!
  var name: Identifier = Identifier.Empty
  var duration: Double = 0.0
  var data: Array[Float] = null
  var timelines: Array[Timeline] = null

  /**
    * Load the animation from a .s2an -file.
    */
  def load(buffer: ByteBuffer): Unit = {

    // @Serialize(s2an)

    // File header
    buffer.verifyMagic("s2an")
    val MaxVersion = 1
    val version = buffer.getVersion(MaxVersion)

    // Animation header
    this.name = buffer.getIdentifier()
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
      timelines(i) = Timeline(bone, baseOffset, numRotKeys, numPosKeys, numSizKeys)
    }

    // Data
    val dataSize = buffer.getInt()
    data = new Array[Float](dataSize)
    buffer.asFloatBuffer.get(this.data)
    buffer.position(buffer.position + dataSize * 4)
  }

}
