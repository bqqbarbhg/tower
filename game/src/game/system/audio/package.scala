package game.system

package object audio {

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
  }

  def unloadGlobal(): Unit = {
    audioSystem.unload()
  }

}
