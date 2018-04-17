package game.system.gameplay

import core._
import game.component._
import game.system._
import game.system.Entity._
import game.system.rendering._
import game.system.rendering.ModelSystem._
import TowerSystem._
import TowerSystemImpl._
import asset.{AssetBundle, AtlasAsset}
import game.system.gameplay.ConnectionSystem.Connection
import game.system.rendering.CullingSystem.RayHit
import ui.{Canvas, DebugDraw}
import ui.SpriteBatch.SpriteDraw
import render._
import render.Renderer._
import util.geometry._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object TowerSystem {

  abstract class Message

  case object MessageNone extends Message
  case class MessageTarget(position: Vector3, time: Double) extends Message

  class Slot(val entity: Entity, val isInput: Boolean, val locale: String, val cablePrefix: String = "") {
    var connectedSlot: Option[Slot] = None
    var connection: Option[Connection] = None
    var prevMessage: Message = MessageNone

    def sendQueued(msg: Message): Unit = {
      for (conn <- connection) conn.sendQueued(msg)
    }
    def sendIfEmpty(msg: Message): Unit = {
      for (conn <- connection) conn.sendIfEmpty(msg)
    }
    def peek: Message = connection.map(_.receive()).getOrElse(MessageNone)
    def pop: Message = {
      val cur = peek
      if (!(cur eq prevMessage)) {
        prevMessage = cur
        cur
      } else {
        MessageNone
      }
    }

    def isEmpty: Boolean = connection.exists(_.isEmpty)

    def detach(): Unit = {
      for (c <- connectedSlot) {
        cableSystem.removeCable(this, c)
        connectionSystem.removeConnection(connection.get)

        c.connection = None
        c.connectedSlot = None
        connectedSlot = None
        connection = None
      }
    }
  }

  val Assets = new AssetBundle("TowerSystem",
    HudAtlas)

}

sealed trait TowerSystem extends EntityDeleteListener {

  /** Add a tower component to the system */
  def addComponent(entity: Entity, comp: Component): Unit

  /** Update all the towers */
  def update(dt: Double): Unit

  /** Update visible entities for rendering */
  def updateVisible(visible: EntitySet): Unit

  /** Retrieve connectable slots from an entity */
  def getSlots(entity: Entity): Seq[Slot]

  /** Try to connect slots `a` and `b`, return whether it was succesful */
  def connectSlots(a: Slot, b: Slot): Boolean

  /** Synced time between towers */
  def time: Double

  /** Register an in-flight target message to show to the player */
  def addTargetMessage(msg: MessageTarget): Unit

  /** Mark position as focused by a turret in the GUI */
  def updateTurretTarget(pos: Vector3): Unit

  /** Render GUI used by this system */
  def renderIngameGui(viewProjection: Matrix4): Unit

}

object TowerSystemImpl {

  abstract class Tower extends CompactArrayPool.Element {
    var next: Tower = null

    def slots: Array[Slot]
    def update(dt: Double): Unit
    def updateVisible(): Unit
  }

  val MaxShootRes = 16
  val sharedShootRes = new ArrayBuffer[RayHit](MaxShootRes)

  class Turret(val entity: Entity, val component: TurretTowerComponent) extends Tower {
    val model = modelSystem.collectModels(entity).head

    var aimNode: Option[NodeInstance] = model.findNode(component.aimBone)
    var spinNode: Option[NodeInstance] = model.findNode(component.spinBone)

    var spin: Double = 0.0
    var spinVel: Double = 0.0

    var aimAngle: Double = 0.0
    var targetAngle: Double = 0.0
    var visualAngle: Double = 0.0
    var visualVel: Double = 0.0

    val slotTargetIn = new Slot(entity, true, "slot.turret.targetIn")

    var targetTime = -100.0
    var targetPos = Vector3.Zero
    var shootTimer: Double = 0.0
    val shootPos = entity.transformPoint(component.shootOrigin)
    var aimTime: Double = 0.0

    override val slots: Array[Slot] = Array(
      slotTargetIn,
    )

    def update(dt: Double): Unit = {

      slotTargetIn.peek match {
        case m: MessageTarget =>
          val pos = entity.inverseTransformPoint(m.position)
          targetTime = m.time
          targetAngle = math.atan2(-pos.x, -pos.z)
          targetPos = m.position

        case _ =>
      }

      var deltaAngle = wrapAngle(targetAngle - aimAngle)
      val turnSpeed = component.turnSpeed * dt
      aimAngle += wrapAngle(clamp(deltaAngle, -turnSpeed, turnSpeed))

      var visualDeltaAngle = wrapAngle(aimAngle - visualAngle)
      val visualSpeed = component.visualTurnSpeed * dt
      visualVel += wrapAngle(clamp(visualDeltaAngle, -visualSpeed, visualSpeed))
      visualVel *= powDt(component.visualTurnFriction, dt)

      visualAngle = wrapAngle(visualAngle + visualVel * dt)

      val time = towerSystem.time
      if (math.abs(deltaAngle) < 0.1 && targetTime + component.shootTime >= time) {
        towerSystem.updateTurretTarget(targetPos)
        spinVel += dt * component.visualSpinSpeed
        aimTime += dt

        if (aimTime >= component.aimDuration - 0.001) {
          shootTimer -= dt
          var shotsDone = 0
          val MaxShotsPerFrame = 10
          while (shootTimer < 0 && shotsDone < MaxShotsPerFrame) {
            shotsDone += 1
            shootTimer += component.shootInterval
            shoot()
          }
        }
      } else {
        aimTime -= dt
      }
      aimTime = clamp(aimTime, 0.0, component.aimDuration)

      spinVel *= powDt(component.visualSpinFriction, dt)

      spin += spinVel * dt
    }

    def shoot(): Unit = {
      val dir = (targetPos - shootPos).normalizeOr { return }
      val ray = Ray(shootPos, dir)

      cullingSystem.queryRay(ray, component.shootDistance, MaxShootRes, CullingSystem.MaskEnemy, sharedShootRes)

      val minT = if (sharedShootRes.nonEmpty) {
        val closest = sharedShootRes.minBy(_.t)
        enemySystem.doDamage(closest.entity, component.shootDamage)
        shootPos.distanceTo(closest.entity.position)
      } else {
        component.shootDistance
      }

      val realT = minT - component.bulletExitDistance
      if (realT > 0.1) {
        val pos = shootPos + dir * component.bulletExitDistance
        val smokePos = shootPos + dir * component.smokeExitDistance
        val size = Vector2(14.0, 8.0)
        val beginColor = Color(1.0, 1.0, 1.0)
        val endColor = Color(0.1, 0.1, 0.1)
        bulletSystem.addBullet(pos, dir * realT, realT * 0.005)
        bulletSystem.addSmoke(smokePos, dir, 0.5, size, beginColor, endColor)
      }

      sharedShootRes.clear()
    }

    override def updateVisible(): Unit = {
      for (node <- aimNode) {
        node.localTransform = Matrix43.rotateZ(visualAngle)
      }
      for (node <- spinNode) {
        node.localTransform = Matrix43.rotateY(spin)
      }
    }

  }

  class RotatingRadar(val entity: Entity, val component: RotatingRadarComponent) extends Tower {
    val model = modelSystem.collectModels(entity).head

    var rotateNode: Option[NodeInstance] = model.findNode(component.rotateBone)

    var angle: Double = 0.0

    val slotTargetOut = new Slot(entity, false, "slot.radar.targetOut")

    override val slots: Array[Slot] = Array(
      slotTargetOut,
    )

    def update(dt: Double): Unit = {
      angle += dt * 2.0

      def findTarget(): Unit = {
        for ((enemy, worldPos) <- enemySystem.queryEnemiesAround(entity.position, component.radius)) {

          val pos = entity.inverseTransformPoint(worldPos)
          val dx = pos.x
          val dz = pos.z
          val lenSq = dx*dx + dz*dz

          if (lenSq > 0.01) {
            val invLen = 1.0 / math.sqrt(lenSq)
            val enemyAngle = math.atan2(-dx, -dz)

            val delta = math.abs(wrapAngle(angle - enemyAngle))
            if (delta <= dt * 4.0 + 0.05) {
              val msg = MessageTarget(worldPos, towerSystem.time)
              slotTargetOut.sendQueued(msg)
              towerSystem.addTargetMessage(msg)
              return
            }

          }
        }
      }

      if (slotTargetOut.isEmpty)
        findTarget()
    }

    override def updateVisible(): Unit = {
      for (node <- rotateNode) {
        node.localTransform = Matrix43.rotateZ(angle)
      }
    }
  }

  class Splitter(val entity: Entity, val component: SplitterComponent) extends Tower {
    val random = new Random()
    val slotInput = new Slot(entity, true, "slot.splitter.input", component.cableNodeIn)
    val slotOutputA = new Slot(entity, false, "slot.splitter.outputA", component.cableNodeOutA)
    val slotOutputB = new Slot(entity, false, "slot.splitter.outputB", component.cableNodeOutB)

    override def slots: Array[Slot] = Array(
      slotInput,
      slotOutputA,
      slotOutputB,
    )

    override def update(dt: Double): Unit = {

      slotInput.pop match {
        case MessageNone =>
        case msg =>

          if (slotOutputA.isEmpty && slotOutputB.isEmpty) {
            if (random.nextBoolean()) {
              slotOutputA.sendQueued(msg)
            } else {
              slotOutputB.sendQueued(msg)
            }
          } else if (slotOutputA.isEmpty) {
            slotOutputA.sendQueued(msg)
          } else if (slotOutputB.isEmpty) {
            slotOutputB.sendQueued(msg)
          }
      }

    }

    override def updateVisible(): Unit = {
    }
  }

  class Merger(val entity: Entity, val component: MergerComponent) extends Tower {
    val random = new Random()
    val slotOutput = new Slot(entity, false, "slot.merger.output", component.cableNodeOut)
    val slotInputA = new Slot(entity, true, "slot.merger.inputA", component.cableNodeInA)
    val slotInputB = new Slot(entity, true, "slot.merger.inputB", component.cableNodeInB)

    override def slots: Array[Slot] = Array(
      slotOutput,
      slotInputA,
      slotInputB,
    )

    override def update(dt: Double): Unit = {

      slotInputA.pop match {
        case MessageNone =>
        case msg => slotOutput.sendIfEmpty(msg)
      }

      slotInputB.pop match {
        case MessageNone =>
        case msg => slotOutput.sendIfEmpty(msg)
      }

    }

    override def updateVisible(): Unit = {
    }
  }

  val NoSlots = Array[Slot]()

  class SlotContainer(val entity: Entity) {
    var slots = new Array[Slot](0)

    def detachAll(): Unit = for (slot <- slots) slot.detach()
  }

  class TargetVisual(val position: Vector3, val time: Double, val addTime: Double)
  class TurretVisual(val addTime: Double) {
    var time: Double = 0.0
  }

  val HudAtlas = AtlasAsset("atlas/hud.s2at")

  val TargetMarkSprite = Identifier("gui/hud/target_mark.png")
  val TurretMarkSprite = Identifier("gui/hud/turret_mark.png")

}

final class TowerSystemImpl extends TowerSystem {

  val towers = new CompactArrayPool[Tower]

  val entityToTower = new mutable.HashMap[Entity, Tower]()
  val entityToSlots = new mutable.HashMap[Entity, SlotContainer]()
  var timeImpl: Double = 0.0

  val visibleTargetVisuals = new ArrayBuffer[TargetVisual]()
  val visibleTurretVisuals = new mutable.HashMap[Vector3, TurretVisual]()
  val turretVisualToDelete = new ArrayBuffer[Vector3]()

  def getSlotContainer(entity: Entity): SlotContainer = {
    entityToSlots.getOrElseUpdate(entity, {
      entity.setFlag(Flag_Slots)
      new SlotContainer(entity)
    })
  }

  override def addComponent(entity: Entity, comp: Component): Unit = {
    val tower = comp match {
      case c: TurretTowerComponent => new Turret(entity, c)
      case c: RotatingRadarComponent => new RotatingRadar(entity, c)
      case c: SplitterComponent => new Splitter(entity, c)
      case c: MergerComponent => new Merger(entity, c)
    }

    if (entity.hasFlag(Flag_Tower)) {
      tower.next = entityToTower(entity)
    }

    val slots = getSlotContainer(entity)
    entity.setFlag(Flag_Tower)
    slots.slots ++= tower.slots
    entityToTower(entity) = tower
    towers.add(tower)
  }

  override def entitiesDeleted(entities: EntitySet): Unit = {
    for (e <- entities.flag(Flag_Tower)) {
      var tower = entityToTower(e)
      do {
        towers.remove(tower)
        tower = tower.next
      } while (tower != null)
      e.clearFlag(Flag_Tower)
    }
    for (e <- entities.flag(Flag_Slots)) {
      val slots = entityToSlots.remove(e).get
      slots.detachAll()
    }
  }

  override def update(dt: Double): Unit = {
    timeImpl += dt
    for (tower <- towers) {
      tower.update(dt)
    }
  }

  override def updateVisible(visible: EntitySet): Unit = {
    for (e <- visible.flag(Flag_Tower)) {
      var tower = entityToTower(e)
      do {
        tower.updateVisible()
        tower = tower.next
      } while (tower != null)
    }
  }

  override def getSlots(entity: Entity): Seq[Slot] = {
    if (entity.hasFlag(Flag_Slots)) {
      entityToSlots(entity).slots
    } else {
      NoSlots
    }
  }

  override def connectSlots(a: Slot, b: Slot): Boolean = {
    if (a == b) return false
    if (a.isInput && b.isInput) return false
    if (!a.isInput && !b.isInput) return false

    val (src, dst) = if (b.isInput) (a, b) else (b, a)

    src.detach()
    dst.detach()

    src.connectedSlot = Some(dst)
    dst.connectedSlot = Some(src)

    val cable = cableSystem.addCable(src, dst)
    val conn = connectionSystem.addConnection(src, dst, cable)

    src.connection = Some(conn)
    dst.connection = Some(conn)

    true
  }

  override def time: Double = timeImpl

  override def addTargetMessage(msg: MessageTarget): Unit = {
    val vis = new TargetVisual(msg.position, msg.time, timeImpl)
    visibleTargetVisuals += vis
  }

  override def updateTurretTarget(pos: Vector3): Unit = {
    val vis = visibleTurretVisuals.getOrElseUpdate(pos, new TurretVisual(timeImpl))
    vis.time = timeImpl
  }

  override def renderIngameGui(viewProjection: Matrix4): Unit = {
    val renderer = Renderer.get
    val sb = Canvas.shared.get.spriteBatch
    val sd = new SpriteDraw()

    renderer.setMode(DepthNone, BlendPremultipliedAlpha, CullNone)

    val size = globalRenderSystem.screenHeight * 0.05

    var ix = 0
    val cutoffDuration = 3.0
    val cutoffTime = timeImpl - cutoffDuration
    while (ix < visibleTargetVisuals.length) {
      val vis = visibleTargetVisuals(ix)
      if (vis.time <= cutoffTime) {
        visibleTargetVisuals(ix) = visibleTargetVisuals.last
        visibleTargetVisuals.trimEnd(1)
      } else {

        val projected = viewProjection.projectPoint(vis.position)
        val x = (projected.x + 1.0) * 0.5 * globalRenderSystem.screenWidth
        val y = (1.0 - (projected.y + 1.0) * 0.5) * globalRenderSystem.screenHeight

        val addT = timeImpl - vis.addTime
        val t = (timeImpl - vis.time) / cutoffDuration
        val startT = math.min(addT * 10.0, 1.0)
        val start = smoothStep(startT * 0.5 + 0.5) * 2.0 - 1.0

        val scale = size * (1.0 + (1.0 - start))

        val alpha = (1.0 - t) * start

        sd.sprite = TargetMarkSprite
        sd.anchorX = 0.5f
        sd.anchorY = 0.5f
        sd.m11 = scale.toFloat
        sd.m22 = scale.toFloat
        sd.m13 = x.toFloat
        sd.m23 = y.toFloat
        sd.color = Color.White.copy(a = alpha)
        sb.draw(sd)

        ix += 1
      }
    }

    val turretCutoffDuration = 0.5
    for ((pos, vis) <- visibleTurretVisuals) {
      val delta = timeImpl - vis.time
      if (delta >= turretCutoffDuration) {
        turretVisualToDelete += pos
      } else {
        val projected = viewProjection.projectPoint(pos)
        val x = (projected.x + 1.0) * 0.5 * globalRenderSystem.screenWidth
        val y = (1.0 - (projected.y + 1.0) * 0.5) * globalRenderSystem.screenHeight

        val addT = timeImpl - vis.addTime
        val t = delta / turretCutoffDuration
        val startT = math.min(addT * 10.0, 1.0)
        val start = smoothStep(startT * 0.5 + 0.5) * 2.0 - 1.0

        val alpha = (1.0 - t) * start

        sd.sprite = TurretMarkSprite
        sd.anchorX = 0.5f
        sd.anchorY = 0.5f
        sd.m11 = size.toFloat
        sd.m22 = size.toFloat
        sd.m13 = x.toFloat
        sd.m23 = y.toFloat
        sd.color = Color.White.copy(a = alpha)
        sb.draw(sd)
      }
    }

    for (pos <- turretVisualToDelete) {
      visibleTurretVisuals.remove(pos)
    }

    turretVisualToDelete.clear()

    sb.flush()
  }
}

