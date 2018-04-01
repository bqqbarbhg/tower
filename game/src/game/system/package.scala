package game

package object system {

  var AudioSystem: AudioSystem = null
  var CableRenderSystem: CableRenderSystem = null

  def load(): Unit = {
    if (AudioSystem != null) {
      AudioSystem.joinAudioThread()
    }

    AudioSystem = new AudioSystem()
    CableRenderSystem = new CableRenderSystem()
  }

  def unload(): Unit = {
    CableRenderSystem.unload()
    AudioSystem.unload()

    CableRenderSystem = null
  }

}
