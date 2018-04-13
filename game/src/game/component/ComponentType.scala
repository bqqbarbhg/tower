package game.component

object ComponentType {

  val Types = Vector(
    BoundingAabbComponent,
    BuildPreviewComponent,
    ModelComponent,
    TurretTowerComponent,
    BuildableComponent,
    GroundBlockerComponent,
  )

}

abstract class ComponentType(val name: String) {
  type Type
  def make: Component
}

