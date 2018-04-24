package game.system.gameplay

import core._
import asset._
import game.component._
import game.system._
import game.system.base._
import EnemySpawnSystemImpl._
import io.property._

import scala.util.Random

sealed trait EnemySpawnSystem {

  /** Set the rounds for the game */
  def setRounds(rounds: Seq[EntityTypeAsset]): Unit

  /** Set seed for the rest of the game */
  def setGameSeed(seed: Int): Unit

  /** Spawn the next enemies, but don't advance yet. Call advanceRound() to move to the next one */
  def spawnNextRound(): Unit

  /** Move to the next round */
  def advanceRound(): Unit

  def persistentState: PropertyContainer

}

object EnemySpawnSystemImpl {
  object PersistentState {
    private val arr = MacroPropertySet.make[PersistentState]()
    private val propertySet: PropertySet = new PropertySet("EnemySpawnSystem.PersistentState", arr)
  }

  class PersistentState extends PropertyContainer {
    override def propertySet: PropertySet = PersistentState.propertySet

    var gameSeed: IntProp.Type = 0
    var roundIndex: IntProp.Type = 0
  }

}

final class EnemySpawnSystemImpl extends EnemySpawnSystem {

  val ps = new PersistentState()
  var rounds: Seq[EntityTypeAsset] = Array[EntityTypeAsset]()

  override def persistentState: PropertyContainer = ps

  override def setRounds(rounds: Seq[EntityTypeAsset]): Unit = {
    this.rounds = rounds
  }

  override def setGameSeed(seed: Int): Unit = {
    ps.gameSeed = seed
  }

  override def advanceRound(): Unit = {
    ps.roundIndex += 1
  }

  override def spawnNextRound(): Unit = {

    for (roundAsset <- rounds.lift(ps.roundIndex)) {
      val random = new Random(ps.gameSeed.toLong << 32 ^ ps.roundIndex.toLong)
      val round = roundAsset.get

      for (spawn <- round.components.collect { case c: EnemySpawnComponent => c }) {
        val proto = spawn.enemyAsset.get

        for (_ <- 0 until spawn.amount) {

          val x = lerp(spawn.areaMin.x, spawn.areaMax.x, random.nextDouble())
          val y = lerp(spawn.areaMin.y, spawn.areaMax.y, random.nextDouble())

          entitySystem.create(proto, Vector3(x, 0.0, y))
        }
      }
    }

  }

}

