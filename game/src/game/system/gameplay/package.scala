package game.system

import game.system.base._

package object gameplay {

  var tutorialSystem: TutorialSystem = null
  var towerSystem: TowerSystem = null

  def loadGame(): Unit = {
    tutorialSystem = new TutorialSystemImpl()
    towerSystem = new TowerSystemImpl()

    entitySystem.addDeleteListener(towerSystem)
  }

  def unloadGame(): Unit = {

    entitySystem.removeDeleteListener(towerSystem)
  }

}
