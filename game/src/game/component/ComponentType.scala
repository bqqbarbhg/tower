package game.component

object ComponentType {

  val Types = Vector(
    BoundingAabbComponent,
    BuildPreviewComponent,
    ModelComponent,
    BuildableComponent,
    GroundBlockerComponent,

    TurretTowerComponent,
    RotatingRadarComponent,
  )

}

abstract class ComponentType(val name: String) {
  type Type
  def make: Component
}

