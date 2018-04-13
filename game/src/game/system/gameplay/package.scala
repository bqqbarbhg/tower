package game.system

import game.system.base._

package object gameplay {

  var tutorialSystem: TutorialSystem = null
  var towerSystem: TowerSystem = null
  var buildSystem: BuildSystem = null
  var cableSystem: CableSystem = null

  def loadGame(): Unit = {
    tutorialSystem = new TutorialSystemImpl()
    towerSystem = new TowerSystemImpl()
    buildSystem = new BuildSystemImpl()
    cableSystem = new CableSystemImpl()

    entitySystem.addDeleteListener(towerSystem)
    entitySystem.addDeleteListener(cableSystem)
  }

  def unloadGame(): Unit = {
    buildSystem.unload()

    entitySystem.removeDeleteListener(towerSystem)
    entitySystem.removeDeleteListener(cableSystem)
  }

}
