package game.component

object ComponentType {

  val Types = Vector(
    BoundingAabbComponent,
    BuildPreviewComponent,
    ModelComponent,
    TurretTowerComponent,
  )

}

abstract class ComponentType(val name: String) {
  def make: Component
}

