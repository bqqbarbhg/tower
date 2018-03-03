package core

object AffineTransform {
  val Identity = AffineTransform(Vector3.Zero, Vector3.One, Quaternion.Identity)

  def lerp(a: AffineTransform, b: AffineTransform, t: Double): AffineTransform = AffineTransform(
    Vector3.lerp(a.position, b.position, t),
    Vector3.lerp(a.scale, b.scale, t),
    Quaternion.lerp(a.rotation, b.rotation, t),
  )
}

case class AffineTransform(position: Vector3, scale: Vector3, rotation: Quaternion) {
}

