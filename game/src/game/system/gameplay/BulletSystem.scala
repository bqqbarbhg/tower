package game.system.gameplay

import core._
import asset._
import BulletSystem._
import BulletSystemImpl._
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

  /** Add a bullet that flies starting from `start` to `distance` in `duration` seconds */
  def addBullet(start: Vector3, distance: Vector3, duration: Double): Unit

  /** Add a smoke effect */
  def addSmoke(position: Vector3, direction: Vector3, duration: Double, size: Vector2, color: Color): Unit

  /** Move the active bullets */
  def updateBullets(dt: Double): Unit

  /** Render active bullets */
  def renderBullets(viewProjection: Matrix4, viewPosition: Vector3): Unit

}

object BulletSystemImpl {
  val BulletAtlas = AtlasAsset("atlas/bullet.s2at")

  val BulletSprite = Identifier("effect/bullet/test.png")

  val SmokeSprites = Array.tabulate(16)(ix => Identifier(s"effect/bullet/smoke_puff.png.$ix"))

  class Bullet(var position: Vector3, var lifetime: Double, val velocity: Vector3, val direction: Vector3)
  class Smoke(val position: Vector3, val direction: Vector3, var time: Double, val duration: Double, val size: Vector2, val color: Color)
}

final class BulletSystemImpl extends BulletSystem {
  val bullets = new ArrayBuffer[Bullet]()
  val smokes = new ArrayBuffer[Smoke]()

  override def addBullet(start: Vector3, distance: Vector3, duration: Double): Unit = {
    val vel = distance / duration
    bullets += new Bullet(start, duration, vel, distance.normalizeOrZero)
  }

  override def addSmoke(position: Vector3, direction: Vector3, duration: Double, size: Vector2, color: Color): Unit = {
    smokes += new Smoke(position, direction, 0.0, duration, size, color)
  }

  override def updateBullets(dt: Double): Unit = {

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
        val color = smoke.color
        val anchor = Vector2(0.1, 0.5)
        val life = math.max((smoke.duration - smoke.time) * 3.0, 0.0)
        val frameTime = smoke.time / smoke.duration * SmokeSprites.length
        val frameI = clamp(frameTime.toInt, 0, SmokeSprites.length - 2)
        val frameF = clamp(frameTime - math.floor(frameTime), 0.0, 1.0)
        bb.drawRight(SmokeSprites(frameI), smoke.position, smoke.direction, size, anchor, color.copy(a = (1.0 - frameF) * life))
        bb.drawRight(SmokeSprites(frameI + 1), smoke.position, smoke.direction, size, anchor, color.copy(a = frameF * life))

        ix += 1
      }
    }

    // Draw bullets
    {
      var ix = 0
      while (ix < bullets.length) {
        val bullet = bullets(ix)

        val size = Vector2(8.0, 1.0)
        val anchor = Vector2(0.8, 0.5)
        bb.drawRight(BulletSprite, bullet.position, bullet.direction, size, anchor, Color.White)

        ix += 1
      }
    }

    bb.flush()
  }
}

