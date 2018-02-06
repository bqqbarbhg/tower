package tower.engine.audio

import scala.collection.mutable.ArrayBuffer

class AudioEngine(val output: AudioOutput) {

  val instances = new ArrayBuffer[SoundInstance]()

  val sampleRate: Int = output.sampleRate

  def render(): Unit = {


  }

}

