package game.component

import asset.EntityTypeAsset
import core._
import game.system.Entity
import game.system.rendering._
import game.system.gameplay._
import io.property._

object EnemySpawnComponent extends ComponentType("EnemySpawn") {
  private val arr = MacroPropertySet.make[EnemySpawnComponent]()
  private val propertySet: PropertySet = new PropertySet("EnemySpawnComponent", arr)
  override def make = new EnemySpawnComponent
  override type Type = EnemySpawnComponent
}

class EnemySpawnComponent extends Component {
  override def propertySet: PropertySet = EnemySpawnComponent.propertySet
  override def componentType = EnemySpawnComponent

  /** Enemy asset to use */
  var enemy: StringProp.Type = ""

  /** How many units of the enemy to spawn */
  var amount: IntProp.Type = 1

  /** Resolve the enemy to use */
  def enemyAsset: EntityTypeAsset = {
    val name = if (enemy.endsWith(".es.toml")) enemy else {
      s"entity/enemy/$enemy.es.toml"
    }

    EntityTypeAsset(name)
  }

  override def create(entity: Entity): Unit = {
  }

}

