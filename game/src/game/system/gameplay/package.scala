package game.system

import game.system.base._

package object gameplay {

  var tutorialSystem: TutorialSystem = null
  var towerSystem: TowerSystem = null
  var buildSystem: BuildSystem = null
  var cableSystem: CableSystem = null
  var connectionSystem: ConnectionSystem = null
  var enemySystem: EnemySystem = null
  var bulletSystem: BulletSystem = null
  var pathfindSystem: PathfindSystem = null

  def loadGame(): Unit = {
    tutorialSystem = new TutorialSystemImpl()
    towerSystem = new TowerSystemImpl()
    buildSystem = new BuildSystemImpl()
    cableSystem = new CableSystemImpl()
    connectionSystem = new ConnectionSystemImpl()
    enemySystem = new EnemySystemImpl()
    bulletSystem = new BulletSystemImpl()
    pathfindSystem = new PathfindSystemImpl()

    entitySystem.addDeleteListener(buildSystem)
    entitySystem.addDeleteListener(towerSystem)
    entitySystem.addDeleteListener(cableSystem)
    entitySystem.addDeleteListener(enemySystem)
  }

  def unloadGame(): Unit = {
    buildSystem.unload()

    entitySystem.removeDeleteListener(buildSystem)
    entitySystem.removeDeleteListener(towerSystem)
    entitySystem.removeDeleteListener(cableSystem)
    entitySystem.removeDeleteListener(enemySystem)
  }

}
