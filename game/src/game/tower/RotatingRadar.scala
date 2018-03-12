package game.tower

import asset.ModelAsset
import core.{Identifier, Matrix43}
import game.system._
import game.tower.RotatingRadar._

object RotatingRadar {

  class Config {
    val model: Identifier = Identifier.Empty
  }

  val NodeRadar = Identifier("Radar")

}

class RotatingRadar(entity: Entity, val config: Config) extends Component(entity) {
  val model = ModelSystem.addModel(entity, config.model)
  val radar = model.findNode(NodeRadar)
  var angle = 0.0

  def update(dt: Double): Unit = {
    angle += dt
    radar.localTransform = Matrix43.rotateX(angle)
  }
}
