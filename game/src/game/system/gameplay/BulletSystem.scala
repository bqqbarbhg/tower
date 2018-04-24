package game.system.gameplay

import core._
import asset._
import BulletSystem._
import BulletSystemImpl._
import game.component.SoundInfo
import game.system._
import game.system.rendering._
import game.system.audio._
import game.system.rendering.AmbientPointLightSystem.AmbientPointLight
import ui.{BillboardBatch, Canvas}
import render._
import render.Renderer._

import scala.collection.mutable.ArrayBuffer

object BulletSystem {

  val Assets = new AssetBundle("BulletSystem",
    BillboardBatch.BillboardShader,
    BillboardBatch.Shared,

    BulletAtlas,
  )

}

sealed trait BulletSystem {

  /** Add a light flash effect */
  def addLightFlash(position: Vector3, intensity: Vector3, radius: Double, duration: Double): Unit

  /** Add a bullet that flies starting from `start` to `distance` in `duration` seconds */
  def addBullet(start: Vector3, distance: Vector3, duration: Double): Unit

  /** Add a smoke effect */
  def addSmoke(position: Vector3, direction: Vector3, duration: Double, size: Vector2, beginColor: Color, endColor: Color): Unit

  /** Add a hit effect */
  def addHit(position: Vector3, direction: Vector3, duration: Double, delay: Double, sound: Option[SoundInfo]): Unit

  /** Move the active bullets */
  def updateBullets(dt: Double): Unit

  /** Render active bullets */
  def renderBullets(viewProjection: Matrix4, viewPosition: Vector3): Unit

}

object BulletSystemImpl {
  val BulletAtlas = AtlasAsset("atlas/bullet.s2at")

  val BulletSprite = Identifier("effect/bullet/test.png")
  val HitSprite = Identifier("effect/bullet/hit.png")

  val SmokeSprites = Array.tabulate(16)(ix => Identifier(s"effect/bullet/smoke_puff.png.$ix"))

  class LightFlash(val entity: Entity, val light: AmbientPointLight, var time: Double, val duration: Double, val intensity: Vector3)
  class Bullet(var position: Vector3, var lifetime: Double, val velocity: Vector3, val direction: Vector3)
  class Smoke(val position: Vector3, val direction: Vector3, var time: Double, val duration: Double, val size: Vector2, val beginColor: Color, val endColor: Color)
  class Hit(val position: Vector3, val direction: Vector3, var time: Double, val duration: Double, val sound: Option[SoundInfo]) {
    var soundPlayed: Boolean = false
  }
}

final class BulletSystemImpl extends BulletSystem {
  val lightFlashes = new ArrayBuffer[LightFlash]()
  val bullets = new ArrayBuffer[Bullet]()
  val smokes = new ArrayBuffer[Smoke]()
  val hits = new ArrayBuffer[Hit]()

  override def addLightFlash(position: Vector3, intensity: Vector3, radius: Double, duration: Double): Unit = {
    val entity = new Entity(true, "Light flash")
    entity.position = position

    val light = ambientPointLightSystem.addLight(entity, Vector3.Zero, intensity, radius)

    lightFlashes += new LightFlash(entity, light, 0.0, duration, intensity)
  }

  override def addBullet(start: Vector3, distance: Vector3, duration: Double): Unit = {
    val vel = distance / duration
    bullets += new Bullet(start, duration, vel, distance.normalizeOrZero)
  }

  override def addSmoke(position: Vector3, direction: Vector3, duration: Double, size: Vector2, beginColor: Color, endColor: Color): Unit = {
    smokes += new Smoke(position, direction, 0.0, duration, size, beginColor, endColor)
  }

  override def addHit(position: Vector3, direction: Vector3, duration: Double, delay: Double, sound: Option[SoundInfo]): Unit = {
    hits += new Hit(position, direction, -delay, duration, sound)
  }

  override def updateBullets(dt: Double): Unit = {

    // Update light flashes
    {
      var ix = 0
      while (ix < lightFlashes.length) {
        val flash = lightFlashes(ix)

        flash.time += dt

        if (flash.time >= flash.duration) {
          flash.entity.delete()
          lightFlashes(ix) = lightFlashes.last
          lightFlashes.trimEnd(1)
        } else {
          val fade = 1.0 - flash.time / flash.duration
          flash.light.intensity = flash.intensity * fade
          ix += 1
        }

      }
    }

    // Update bullets
    {
      var ix = 0
      while (ix < bullets.length) {
        val bullet = bullets(ix)
        bullet.lifetime -= dt

        if (bullet.lifetime <= 0.0) {
          bullets(ix) = bullets.last
          bullets.trimEnd(1)
        } else {
          bullet.position += bullet.velocity * dt
          ix += 1
        }
      }
    }

    // Update smokes
    {
      var ix = 0
      while (ix < smokes.length) {
        val smoke = smokes(ix)
        smoke.time += dt

        if (smoke.time >= smoke.duration) {
          smokes(ix) = smokes.last
          smokes.trimEnd(1)
        } else {
          ix += 1
        }
      }
    }

    // Update hits
    {
      var ix = 0
      while (ix < hits.length) {
        val hit = hits(ix)
        hit.time += dt

        if (hit.time >= 0.0 && !hit.soundPlayed) {
          hit.soundPlayed = true
          for (sound <- hit.sound)
            sceneAudioSystem.play(hit.position, sound)
        }

        if (hit.time >= hit.duration) {
          hits(ix) = hits.last
          hits.trimEnd(1)
        } else {
          ix += 1
        }
      }
    }
  }

  override def renderBullets(viewProjection: Matrix4, viewPosition: Vector3): Unit = {
    val bb = BillboardBatch.Shared.get
    val renderer = Renderer.get

    renderer.setMode(DepthTest, BlendPremultipliedAlpha, CullNone)

    bb.viewProjection = viewProjection
    bb.cameraPosition = viewPosition

    // Draw smokes
    {
      var ix = 0
      while (ix < smokes.length) {
        val smoke = smokes(ix)

        val size = smoke.size
        val anchor = Vector2(0.1, 0.5)

        val relTime = smoke.time / smoke.duration
        val frameTime = relTime * (SmokeSprites.length - 1)
        val frameI = clamp(frameTime.toInt, 0, SmokeSprites.length - 2)
        val frameF = clamp(frameTime - math.floor(frameTime), 0.0, 1.0)

        val color = Color.lerp(smoke.beginColor, smoke.endColor, relTime)

        bb.drawRight(SmokeSprites(frameI), smoke.position, smoke.direction, size, anchor, color.copy(a = 1.0 - frameF))
        bb.drawRight(SmokeSprites(frameI + 1), smoke.position, smoke.direction, size, anchor, color.copy(a = frameF))

        ix += 1
      }
    }

    // Draw hits
    {
      var ix = 0
      while (ix < hits.length) {
        val hit = hits(ix)

        if (hit.time >= 0.0) {
          val relT = hit.time / hit.duration

          val size = Vector2(2.0 + 8.0 * relT, 1.3 - relT * 1.0)
          val anchor = Vector2(0.5, 0.5)
          val alpha = math.min((1.0 - relT) * 2.0, 1.0)

          bb.drawRight(HitSprite, hit.position, hit.direction, size, anchor, Color.White.copy(a = alpha))
        }

        ix += 1
      }
    }

    // Draw bullets
    {
      var ix = 0
      while (ix < bullets.length) {
        val bullet = bullets(ix)

        val size = Vector2(8.0, 0.5)
        val anchor = Vector2(0.8, 0.5)
        bb.drawRight(BulletSprite, bullet.position, bullet.direction, size, anchor, Color.White)

        ix += 1
      }
    }

    bb.flush()
  }
}

