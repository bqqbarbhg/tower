package game.component

object ComponentType {

  val Types = Vector(
    BoundingAabbComponent,
    BuildPreviewComponent,
    ModelComponent,
    BuildableComponent,
    GroundBlockerComponent,
    EnemyComponent,

    TurretTowerComponent,
    RotatingRadarComponent,
    SplitterComponent,
    MergerComponent,
  )

}

abstract class ComponentType(val name: String) {
  type Type
  def make: Component
}

