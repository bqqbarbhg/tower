package game.component

object ComponentType {

  val Types = Vector(
    BoundingAabbComponent,
    BuildPreviewComponent,
    ModelComponent,
    BuildableComponent,
    GroundBlockerComponent,
    EnemyComponent,
    EnemyTargetComponent,
    EnemyBlockerComponent,

    TurretTowerComponent,
    RotatingRadarComponent,
    SplitterComponent,
    MergerComponent,
    WallDoorComponent,
    DrillTowerComponent,
    BreakableComponent,

    DebrisComponent,
  )

}

abstract class ComponentType(val name: String) {
  type Type
  def make: Component
}

