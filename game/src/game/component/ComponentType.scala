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
    LightProbeComponent,
    EnemySpawnComponent,
    GridBlockComponent,

    TurretTowerComponent,
    RotatingRadarComponent,
    SplitterComponent,
    MergerComponent,
    WallDoorComponent,
    DrillTowerComponent,

    BreakableComponent,
    DestroyableTowerComponent,

    DebrisComponent,
  )

}

abstract class ComponentType(val name: String) {
  type Type
  def make: Component
}

