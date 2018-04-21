package game.system.rendering

import scala.collection.mutable.ArrayBuffer
import core._
import AmbientSystem.Probe
import game.options.Options
import render._
import util.geometry._

sealed trait DirectionalLightSystem {

  /**
    * Set the global directional light
    */
  def setLight(direction: Vector3, intensity: Vector3): Unit

  /**
    * Add point-light to visible total ambient probes.
    */
  def updateVisibleProbes(probes: ArrayBuffer[Probe]): Unit

  /**
    * Create shadow view-projection from directional light.
    */
  def setupShadowProjection(eyePosition: Vector3, lookDirection: Vector3): Unit

  /**
    * Current view-projection matrix for the shadow.
    */
  def shadowViewProjection: Matrix4

  /**
    * Render target to use for shadow mapping
    */
  def shadowTarget: RenderTarget

  /**
    * Current light direction (towards the light)
    */
  def lightDirection: Vector3

  /**
    * Current light intensity
    */
  def lightIntensity: Vector3

  /**
    * Release resources used by this system.
    */
  def unload(): Unit

}

final class DirectionalLightSystemImpl extends DirectionalLightSystem {

  var _lightDirection: Vector3 = Vector3.Zero
  var _lightIntensity: Vector3 = Vector3.Zero
  var viewProj: Matrix4 = Matrix4.Identity

  override def lightDirection = _lightDirection
  override def lightIntensity = _lightIntensity

  val shadowTargetSize = if (Options.current.graphics.quality.shaderQuality >= 3) 2048
  else if (Options.current.graphics.quality.shaderQuality >= 2) 1024
  else 512

  override val shadowTarget = RenderTarget.create(shadowTargetSize, shadowTargetSize, None, Some(TexFormat.D24S8), true)

  val groundPlane = Plane(Vector3.Up, 0.0)

  override def setLight(direction: Vector3, intensity: Vector3): Unit = {
    _lightDirection = direction.normalizeOrZero
    _lightIntensity = intensity
  }

  override def updateVisibleProbes(probes: ArrayBuffer[Probe]): Unit = {
    var ix = 0
    val len = probes.length
    while (ix < len) {
      val probe = probes(ix)
      assert(probe.gatherTotal, "Directional probes must gather all light")

      probe.totalIrradianceProbe.addDirectional(_lightDirection, _lightIntensity)

      ix += 1
    }
  }

  override def setupShadowProjection(eyePosition: Vector3, lookDirection: Vector3): Unit = {
    val eyeRay = Ray(eyePosition, lookDirection)
    val groundPoint = eyeRay.point(eyeRay.intersect(groundPlane).getOrElse(0.0))

    val size = 140.0
    val texelPerUnit = shadowTargetSize.toDouble / size

    val forward = _lightDirection
    val right = (forward cross Vector3.Up).normalize
    val up = (forward cross right).normalize

    val targetPos = groundPoint + _lightDirection * 80.0

    val targetFwd = (targetPos dot forward)
    val targetUp = (targetPos dot up)
    val targetRight = (targetPos dot right)

    val fixedUp = math.round(targetUp * texelPerUnit) / texelPerUnit
    val fixedRight = math.round(targetRight * texelPerUnit) / texelPerUnit

    val fixedPos = forward * targetFwd + up * fixedUp + right * fixedRight

    val view = Matrix43.look(fixedPos, -_lightDirection, Vector3.Up)
    val proj = Matrix4.orthographic(size, size, 1.0, 160.0)

    viewProj = proj * view
  }

  /**
    * Current view-projection matrix for the shadow.
    */
  override def shadowViewProjection = viewProj

  override def unload(): Unit = {
    shadowTarget.unload()
  }
}

