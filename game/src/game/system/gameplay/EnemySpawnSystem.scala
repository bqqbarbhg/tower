package game.system.gameplay

import core._
import asset._
import game.component._
import game.system._
import game.system.base._

sealed trait EnemySpawnSystem {

  def spawnNextRound(): Unit

}

final class EnemySpawnSystemImpl extends EnemySpawnSystem {

  override def spawnNextRound(): Unit = {

    val round = EntityTypeAsset("entity/spawn/test_round.es.toml").get

    for (spawn <- round.components.collect { case c: EnemySpawnComponent => c }) {
      val proto = spawn.enemyAsset.get

      for (_ <- 0 until spawn.amount) {
        entitySystem.create(proto, Vector3(math.random() * 30.0, 0.0, -50.0 - math.random() * 20.0))
      }
    }

  }

}

