package game.system

import game.system.base._

package object gameplay {

  var tutorialSystem: TutorialSystem = null
  var towerSystem: TowerSystem = null
  var buildSystem: BuildSystem = null

  def loadGame(): Unit = {
    tutorialSystem = new TutorialSystemImpl()
    towerSystem = new TowerSystemImpl()
    buildSystem = new BuildSystemImpl()

    entitySystem.addDeleteListener(towerSystem)
  }

  def unloadGame(): Unit = {
    buildSystem.unload()

    entitySystem.removeDeleteListener(towerSystem)
  }

}
