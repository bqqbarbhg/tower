package game.system

package object audio {

  var audioIsLoaded: Boolean = false
  var audioSystem: AudioSystem = null

  def joinAudioThread(): Unit = {
    if (audioSystem != null) {
      audioSystem.joinAudioThread()
      audioSystem = null
    }
  }

  def loadGlobal(): Unit = {
    assert(audioSystem == null)
    audioSystem = new AudioSystemImpl()
    audioIsLoaded = true
  }

  def unloadGlobal(): Unit = {
    audioIsLoaded = false
    audioSystem.unload()
  }

}
