package game.options

import io.{SimpleSerializable, SimpleVisitor}

class AudioOptions extends SimpleSerializable {

  /** Audio frames per second */
  var sampleRate: Int = 44100

  /** Latency of the audio in seconds */
  var latency: Double = 0.1

  /** File to save debug output into */
  var debugFilename: String = ""

  /** Audio output device to use */
  var audioOutput: String = "OpenAL"

  /** Use audio debugging */
  var debug: Boolean = false

  override def visit(v: SimpleVisitor): Unit = {
    sampleRate = v.field("sampleRate", sampleRate)
    latency = v.field("latency", latency)
    debugFilename = v.field("debugFilename", debugFilename)
    audioOutput = v.field("audioOutput", audioOutput)
    debug = v.field("debug", debug)
  }

}

