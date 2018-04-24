package game.system

import game.system.base._

package object audio {

  var audioIsLoaded: Boolean = false
  var audioSystem: AudioSystem = null
  var sceneAudioSystem: SceneAudioSystem = null

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

  def loadState(): Unit = {
    sceneAudioSystem = new SceneAudioSystemImpl()
  }

  def unloadState(): Unit = {
    sceneAudioSystem.unload()
  }

}
