package game.system

package object audio {

  var audioSystem: AudioSystem = null

  def load(): Unit = {

    audioSystem = new AudioSystemImpl()

  }

}
