package game.system.gameplay

import core._
import game.component._
import game.system._
import game.system.Entity._
import game.system.base._
import game.system.rendering._
import game.system.rendering.ModelSystem._
import TowerSystem._
import TowerSystemImpl._
import asset.{AssetBundle, AtlasAsset, EntityTypeAsset}
import game.system.gameplay.ConnectionSystem.Connection
import game.system.rendering.AmbientPointLightSystem.AmbientPointLight
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
  case class MessageTarget(position: Vector3, velocity: Vector3, time: Double) extends Message {

    def positionAt(currentTime: Double): Vector3 = position + velocity * (currentTime - time)

  }

  class Slot(val entity: Entity, val info: SlotInfo) {
    def isInput: Boolean = info.isInput

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

    def worldPosition: Vector3 = entity.transformPoint(info.offset)
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
  def updateTurretTarget(turret: Entity, pos: Vector3): Unit

  /** Render GUI used by this system */
  def renderIngameGui(viewProjection: Matrix4): Unit

  /** Register an entity as destroyable */
  def addDestroyableTower(entity: Entity, component: DestroyableTowerComponent): Unit

  /** Deal damage to a tower */
  def doDamage(entity: Entity, amount: Double): Unit

  /** Reset towers back to build mode */
  def resetTowers(): Unit

}

object TowerSystemImpl {

  abstract class Tower extends CompactArrayPool.Element {
    var next: Tower = null

    def slots: Array[Slot]
    def update(dt: Double): Unit
    def updateVisible(): Unit

    def reset(): Unit = { }
  }

  val MaxShootRes = 16
  val sharedShootRes = new ArrayBuffer[RayHit](MaxShootRes)
  val sharedRandom = new Random()

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

    val slotTargetIn = new Slot(entity, component.targetIn)

    var targetTime = -100.0
    var targetPos = Vector3.Zero
    var shootTimer: Double = 0.0
    val shootPos = entity.transformPoint(component.shootOrigin)
    var aimTime: Double = 0.0

    var yawTarget: Double = 0.0
    var visualYaw: Double = 0.0

    var ambientLight: Option[AmbientPointLight] = None

    override def reset(): Unit = {
      targetAngle = 0.0
      aimAngle = 0.0
      visualVel = 0.0
      visualAngle = 0.0
      visualYaw = 0.0
      targetTime = -100.0
    }

    override val slots: Array[Slot] = Array(
      slotTargetIn,
    )

    def update(dt: Double): Unit = {
      val time = towerSystem.time

      slotTargetIn.peek match {
        case m: MessageTarget =>
          val worldPos = m.positionAt(time)
          val pos = entity.inverseTransformPoint(worldPos)
          targetTime = m.time
          targetAngle = math.atan2(-pos.x, -pos.z)
          targetPos = worldPos

        case _ =>
      }

      {
        val dx = targetPos.distanceTo(shootPos.copy(y = targetPos.y))
        val dy = targetPos.y - shootPos.y

        val maxYawLinear = component.visualYawExponential * dt

        yawTarget = math.atan2(dy, dx)
        visualYaw += (yawTarget - visualYaw) * powDt(component.visualYawExponential, dt)
        visualYaw += clamp(yawTarget - visualYaw, -maxYawLinear, maxYawLinear)
      }

      var deltaAngle = wrapAngle(targetAngle - aimAngle)
      val turnSpeed = component.turnSpeed * dt

      if (targetTime + component.shootTime >= time) {
        aimAngle += wrapAngle(clamp(deltaAngle, -turnSpeed, turnSpeed))
      }

      var visualDeltaAngle = wrapAngle(aimAngle - visualAngle)
      val visualSpeed = component.visualTurnSpeed * dt
      visualVel += wrapAngle(clamp(visualDeltaAngle, -visualSpeed, visualSpeed))
      visualVel *= powDt(component.visualTurnFriction, dt)

      visualAngle = wrapAngle(visualAngle + visualVel * dt)

      if (math.abs(deltaAngle) < 0.1 && targetTime + component.shootTime >= time) {
        towerSystem.updateTurretTarget(entity, targetPos)
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
      val originalDir = (targetPos - shootPos).normalizeOr { return }
      val dir = (originalDir + Vector3(
        (sharedRandom.nextDouble - 0.5) * component.spread.x,
        (sharedRandom.nextDouble - 0.5) * component.spread.y,
        (sharedRandom.nextDouble - 0.5) * component.spread.z,
      )).normalizeOr { return }
      val ray = Ray(shootPos, dir)

      cullingSystem.queryRay(ray, component.shootDistance, MaxShootRes, CullingSystem.MaskEnemy, sharedShootRes)

      val hit = sharedShootRes.nonEmpty
      var rayT = 0.0
      var rayEntity: Option[Entity] = None
      val minT = if (hit) {
        val closest = sharedShootRes.minBy(_.t)
        rayT = closest.t
        rayEntity = Some(closest.entity)
        shootPos.distanceTo(closest.entity.position)
      } else {
        component.shootDistance
      }


      val realT = minT - component.bulletExitDistance
      val bulletDuration = realT * 0.005

      for (enemy <- rayEntity) {
        enemySystem.doDamageDelayed(enemy, component.shootDamage, bulletDuration)
      }

      if (realT > 0.1) {
        val pos = shootPos + dir * component.bulletExitDistance
        val smokePos = shootPos + dir * component.smokeExitDistance
        val size = Vector2(14.0, 8.0)
        val beginColor = Color(1.0, 1.0, 1.0)
        val endColor = Color(0.1, 0.1, 0.1)
        bulletSystem.addBullet(pos, dir * realT, bulletDuration)
        bulletSystem.addSmoke(smokePos, dir, 0.5, size, beginColor, endColor)
        bulletSystem.addLightFlash(pos, Vector3.One * 0.1, 35.0, 0.3)

        if (hit) {
          val random = Vector3(sharedRandom.nextDouble - 0.5, sharedRandom.nextDouble - 0.5, sharedRandom.nextDouble - 0.5).normalizeOrZero
          val right = (dir cross random).normalizeOrZero
          val up = (right cross dir).normalizeOrZero
          val t = rayT + 0.5
          bulletSystem.addHit(shootPos + dir * t, right, 0.1, bulletDuration)
          bulletSystem.addHit(shootPos + dir * t, up, 0.1, bulletDuration)
        }
      }

      sharedShootRes.clear()
    }

    override def updateVisible(): Unit = {
      for (node <- aimNode) {
        node.localTransform = Matrix43.rotateZ(visualAngle) * Matrix43.rotateX(visualYaw)
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

    val slotTargetOut = new Slot(entity, component.targetOut)

    override def reset(): Unit = {
      angle = 0.0
    }

    override val slots: Array[Slot] = Array(
      slotTargetOut,
    )

    def update(dt: Double): Unit = {
      angle += dt * component.rotateSpeed

      def findTarget(): Unit = {
        for (target <- enemySystem.queryEnemiesAround(entity.position, component.radius)) {

          val pos = entity.inverseTransformPoint(target.position)
          val dx = pos.x
          val dz = pos.z
          val lenSq = dx*dx + dz*dz

          if (lenSq > 0.01) {
            val invLen = 1.0 / math.sqrt(lenSq)
            val enemyAngle = math.atan2(-dx, -dz)

            val delta = math.abs(wrapAngle(angle - enemyAngle))
            if (delta <= dt * 4.0 + 0.05) {
              val msg = MessageTarget(target.position, target.velocity, towerSystem.time)
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
    val slotInput = new Slot(entity, component.input)
    val slotOutputA = new Slot(entity, component.outputA)
    val slotOutputB = new Slot(entity, component.outputB)

    override val slots: Array[Slot] = Array(
      slotInput,
      slotOutputA,
      slotOutputB,
    )

    override def update(dt: Double): Unit = {

      slotInput.pop match {
        case MessageNone =>
        case msg =>

          if (slotOutputA.isEmpty && slotOutputB.isEmpty) {
            if (sharedRandom.nextBoolean()) {
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
    val slotOutput = new Slot(entity, component.output)
    val slotInputA = new Slot(entity, component.inputA)
    val slotInputB = new Slot(entity, component.inputB)

    override val slots: Array[Slot] = Array(
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

  class WallDoor(val entity: Entity, val component: WallDoorComponent) extends Tower {
    val model = modelSystem.collectModels(entity).head
    var doorNode: Option[NodeInstance] = model.findNode(component.doorBone)

    val slotOpen = new Slot(entity, component.open)

    var isOpen = false
    var openAmount: Double = 0.0

    var blockShapeSerial = -1
    val blockAabb = Aabb.fromMinMax(component.blockMin, component.blockMax)
    var closedTimer: Double = 0.0

    def createBlockShape(): Unit = {
      if (blockShapeSerial >= 0) return
      blockShapeSerial = cullingSystem.addAabb(entity, blockAabb, CullingSystem.MaskEnemy)
    }

    def removeBlockShape(): Unit = {
      if (blockShapeSerial < 0) return
      cullingSystem.removeShape(entity, blockShapeSerial)
      blockShapeSerial = -1
    }

    override val slots: Array[Slot] = Array(
      slotOpen,
    )

    override def update(dt: Double): Unit = {

      if (closedTimer >= 0.0)
        closedTimer -= dt

      if (slotOpen.connection.isDefined) {

        slotOpen.pop match {
          case MessageNone =>
          case msg => closedTimer = component.closeTime
        }

        isOpen = closedTimer <= 0.01

      } else {
        isOpen = false
      }

      if (isOpen) {
        openAmount += dt / component.openDuration
      } else {
        openAmount -= dt / component.openDuration
      }

      if (openAmount > 0.5) {
        removeBlockShape()
      } else {
        createBlockShape()
      }

      openAmount = clamp(openAmount, 0.0, 1.0)
    }

    override def updateVisible(): Unit = {
      for (node <- doorNode) {
        val t = smoothStep(openAmount)
        node.localTransform = Matrix43.translate(component.moveAmount * t)
      }
    }
  }

  object Drill {
    val IdleAnim = Identifier("Idle")
    val DrillAnim = Identifier("Drill")
  }

  class Drill(val entity: Entity, val component: DrillTowerComponent) extends Tower {
    import Drill._

    val model = modelSystem.collectModels(entity).head
    var rotateNode: Option[NodeInstance] = model.findNode(component.rotateBone)
    val animator = animationSystem.addAnimator(entity, model)

    animator.playLoop(0, IdleAnim)

    var prevAngle = 0.0
    var turnAmount = 0.0
    var turnTime = Double.MaxValue

    var rotation = 0.0

    var animation: Option[AnimationSystem.AnimationChannel] = None

    override val slots: Array[Slot] = Array()

    override def update(dt: Double): Unit = {
      for (anim <- animation) {
        if (anim.isDone) {
          animation = None
        }
        return
      }

      if (turnTime >= component.turnDuration + component.turnWaitTime) {

        if (sharedRandom.nextDouble <= component.drillChance) {
          animation = Some(animator.playOnce(1, DrillAnim, 0.1, 0.1))
        } else {
          turnTime = 0.0
          prevAngle = wrapAngle(rotation)
          turnAmount = (sharedRandom.nextDouble() * 2.0 - 1.0) * math.Pi
        }

      } else {
        turnTime += dt
        val relT = turnTime / component.turnDuration
        val t = smoothStep(clamp(relT, 0.0, 1.0))
        rotation = prevAngle + turnAmount * t
      }

    }

    override def updateVisible(): Unit = {
      for (node <- rotateNode) {
        node.localTransform = Matrix43.rotateZ(rotation)
      }
    }
  }

  val NoSlots = Array[Slot]()

  class SlotContainer(val entity: Entity) {
    var slots = new Array[Slot](0)

    def detachAll(): Unit = for (slot <- slots) slot.detach()
  }

  class TargetVisual(val msg: MessageTarget, val addTime: Double)
  class TurretVisual(val addTime: Double) {
    var time: Double = 0.0
    var position: Vector3 = Vector3.Zero
  }

  val HudAtlas = AtlasAsset("atlas/hud.s2at")

  val TargetMarkSprite = Identifier("gui/hud/target_mark.png")
  val TurretMarkSprite = Identifier("gui/hud/turret_mark.png")

  class DestroyableTower(val entity: Entity, val component: DestroyableTowerComponent) {
    var health: Double = component.health
  }

}

final class TowerSystemImpl extends TowerSystem {

  val towers = new CompactArrayPool[Tower]

  val entityToTower = new mutable.HashMap[Entity, Tower]()
  val entityToSlots = new mutable.HashMap[Entity, SlotContainer]()
  val entityToDestroyable = new mutable.HashMap[Entity, DestroyableTower]()

  var timeImpl: Double = 0.0

  val visibleTargetVisuals = new ArrayBuffer[TargetVisual]()
  val visibleTurretVisuals = new mutable.HashMap[Entity, TurretVisual]()
  val turretVisualToDelete = new ArrayBuffer[Entity]()

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
      case c: WallDoorComponent => new WallDoor(entity, c)
      case c: DrillTowerComponent => new Drill(entity, c)
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
      e.clearFlag(Flag_Slots)
    }

    for (e <- entities.flag(Flag_DestroyableTower)) {
      entityToDestroyable.remove(e)
      e.clearFlag(Flag_DestroyableTower)
    }

  }

  override def update(dt: Double): Unit = {
    timeImpl += dt

    if (!pauseSystem.paused) {
      for (tower <- towers) {
        tower.update(dt)
      }
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
    val vis = new TargetVisual(msg, timeImpl)
    visibleTargetVisuals += vis
  }

  override def updateTurretTarget(turret: Entity, pos: Vector3): Unit = {
    val vis = visibleTurretVisuals.getOrElseUpdate(turret, new TurretVisual(timeImpl))
    vis.position = pos
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
      if (vis.msg.time <= cutoffTime) {
        visibleTargetVisuals(ix) = visibleTargetVisuals.last
        visibleTargetVisuals.trimEnd(1)
      } else {

        val pos = vis.msg.positionAt(timeImpl)
        val projected = viewProjection.projectPoint(pos)
        val x = (projected.x + 1.0) * 0.5 * globalRenderSystem.screenWidth
        val y = (1.0 - (projected.y + 1.0) * 0.5) * globalRenderSystem.screenHeight

        val addT = timeImpl - vis.addTime
        val t = (timeImpl - vis.msg.time) / cutoffDuration
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
    for ((entity, vis) <- visibleTurretVisuals) {
      val delta = timeImpl - vis.time
      if (delta >= turretCutoffDuration) {
        turretVisualToDelete += entity
      } else {
        val projected = viewProjection.projectPoint(vis.position)
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

    for (entity <- turretVisualToDelete) {
      visibleTurretVisuals.remove(entity)
    }

    turretVisualToDelete.clear()

    sb.flush()
  }

  override def addDestroyableTower(entity: Entity, component: DestroyableTowerComponent): Unit = {
    val destroyable = new DestroyableTower(entity, component)
    entityToDestroyable(entity) = destroyable

    entity.setFlag(Flag_DestroyableTower)
  }

  override def doDamage(entity: Entity, amount: Double): Unit = {
    if (!entity.hasFlag(Flag_DestroyableTower)) return

    val destroyable = entityToDestroyable(entity)
    destroyable.health -= amount

    if (destroyable.health <= 0.0) {
      for (breakable <- entity.prototype.find(BreakableComponent)) {
        val et = EntityTypeAsset(breakable.hardBreakEffect)
        entitySystem.createEffect(et.get, entity.position, entity.rotation)
      }

      entity.delete()
    }
  }

  override def resetTowers(): Unit = {
    for ((entity, destroyable) <- entityToDestroyable) {
      destroyable.health = destroyable.component.health
    }

    for (tower <- towers) {
      tower.reset()
    }
  }

}

