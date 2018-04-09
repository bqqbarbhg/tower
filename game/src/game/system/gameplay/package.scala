package game.system

package object gameplay {

  var tutorialSystem: TutorialSystem = null

  def loadGame(): Unit = {
    tutorialSystem = new TutorialSystemImpl()
  }

  def unloadGame(): Unit = {
  }

}
