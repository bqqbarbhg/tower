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
  var pauseSystem: PauseSystem = null
  var enemySpawnSystem: EnemySpawnSystem = null
  var saveStateSystem: SaveStateSystem = null

  def loadGame(): Unit = {
    tutorialSystem = new TutorialSystemImpl()
    towerSystem = new TowerSystemImpl()
    buildSystem = new BuildSystemImpl()
    cableSystem = new CableSystemImpl()
    connectionSystem = new ConnectionSystemImpl()
    enemySystem = new EnemySystemImpl()
    bulletSystem = new BulletSystemImpl()
    pathfindSystem = new PathfindSystemImpl()
    pauseSystem = new PauseSystemImpl()
    enemySpawnSystem = new EnemySpawnSystemImpl()
    saveStateSystem = new SaveStateSystemImpl()

    entitySystem.addDeleteListener(buildSystem)
    entitySystem.addDeleteListener(towerSystem)
    entitySystem.addDeleteListener(cableSystem)
    entitySystem.addDeleteListener(enemySystem)
    entitySystem.addDeleteListener(saveStateSystem)
  }

  def unloadGame(): Unit = {
    buildSystem.unload()

    entitySystem.removeDeleteListener(buildSystem)
    entitySystem.removeDeleteListener(towerSystem)
    entitySystem.removeDeleteListener(cableSystem)
    entitySystem.removeDeleteListener(enemySystem)
    entitySystem.removeDeleteListener(saveStateSystem)
  }

}
