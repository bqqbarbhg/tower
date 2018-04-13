package game.system.gameplay

import core._
import asset.ModelAsset
import game.component.ModelComponent
import game.system._
import game.system.Entity._
import game.system.base._
import game.system.rendering._
import game.system.gameplay.TowerSystem.Slot
import game.system.rendering.CableRenderSystem.CableNode
import CableSystem._
import CableSystemImpl._
import io.property._
import ui.DebugDraw
import util._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object CableSystem {

  sealed abstract class Cable {
    def entity: Entity
  }

  object CableTweak extends PropertyContainer {
    private val arr = MacroPropertySet.make[CableTweak.type]()
    override val propertySet: PropertySet = new PropertySet("CableTweak", arr) {
      range("positionRandom", 0.0, 2.0)
      range("angleRandom", 0.0, 2.0)
      range("tangentScaleMid", 0.0, 1.0)
      range("tangentScaleEndpoint", 0.0, 1.0)
    }

    var positionRandom: DoubleProp.Type = 0.2
    var angleRandom: DoubleProp.Type = 0.2
    var tangentScaleMid: DoubleProp.Type = 0.5
    var tangentScaleEndpoint: DoubleProp.Type = 0.5
    var avoidWeight: DoubleProp.Type = 10.0
    var regenerateCables: BoolProp.Type = false
  }

}

sealed trait CableSystem extends EntityDeleteListener {

  /** Add a new cable */
  def addCable(src: Slot, dst: Slot): Cable

  /** Block cables from going through an object */
  def addGroundBlocker(entity: Entity, min: Vector2, max: Vector2): Unit

  /** Render debug view of cable control points */
  def debugDrawControlPoints(visible: EntitySet)

  /** Render debug view of ground blockers */
  def debugDrawGroundBlockers()

  /** Regenerate all the cables */
  def regenerateAllCables(): Unit

}

object CableSystemImpl {

  final class CableImpl(val src: Slot, val dst: Slot) extends Cable {
    var entityImpl: Entity = _
    var nodes: Array[CableNode] = _

    override def entity: Entity = entityImpl
  }

  val DummyPaths = Array(Array(CableNode(Vector3.Zero, Vector3.Zero)))

  def reverseNodes(nodes: Array[CableNode]): Array[CableNode] = {
    nodes.reverse.map(n => n.copy(tangentIn = -n.tangentIn, tangentOut = -n.tangentOut))
  }

  val CellSize = Vector2(8.0, 8.0)
  val CellArea = CellSize.x * CellSize.y
  val CellInvArea = 1.0 / CellArea

  val BlockMultiplier = 10000
  val InvBlockMultiplier = 1.0 / BlockMultiplier.toDouble

  class GroundCell(val x: Int, val y: Int) {
    var blockCount: Int = 0
    var blockAmount: Int = 0
    val minWorld = Vector2(x * CellSize.x, y * CellSize.y)
    val maxWorld = Vector2((x + 1) * CellSize.x, (y + 1) * CellSize.y)

    var blockers = new Array[GroundBlocker](0)

    def moveWeight: Double = blockAmount.toDouble * InvBlockMultiplier

    def testPoint(a: Vector2): Boolean = {
      for (blocker <- blockers) {
        val min = blocker.minWorld
        val max = blocker.maxWorld
        if (a.x >= min.x && a.y >= min.y && a.x <= max.x && a.y <= max.y)
          return false
      }

      true
    }

    /** Test if line segment intersects with any blockers.
      * Returns true if clear. */
    def testLineSegment(a: Vector2, b: Vector2): Boolean = {
      val diff = (b - a)
      val diffLen = diff.length
      if (diffLen < 0.001) return true
      val d = diff / diffLen
      val idx = 1.0 / d.x
      val idy = 1.0 / d.y

      for (blocker <- blockers) {
        var t1 = blocker.minWorld.x * idx
        var t2 = blocker.maxWorld.x * idx

        var tMin = math.min(t1, t2)
        var tMax = math.max(t1, t2)

        t1 = blocker.minWorld.y * idy
        t2 = blocker.maxWorld.y * idy

        tMin = math.max(tMin, math.min(t1, t2))
        tMax = math.min(tMax, math.max(t1, t2))

        if (tMin <= tMax && tMax >= 0.0 && tMax <= diffLen) {
          return false
        }
      }

      true
    }
  }

  class GroundBlocker(val minX: Int, val minY: Int, val maxX: Int, val maxY: Int, val minWorld: Vector2, val maxWorld: Vector2, val next: GroundBlocker) {
  }

}

final class CableSystemImpl extends CableSystem {
  val slotToCable = new mutable.HashMap[Slot, CableImpl]()
  val entityToCable = new mutable.HashMap[Entity, CableImpl]()
  val entityToGroundBlocker = new mutable.HashMap[Entity, GroundBlocker]()
  val random = new Random()
  val cells = new SparseGrid[GroundCell](CellSize, (x, y) => new GroundCell(x, y))

  case class GroundSearchState(x: Int, y: Int, gx: Int, gy: Int) extends AStar.State[GroundSearchState] {
    def weight: Double = cells.getCell(x, y).map(c => 1.0 + c.moveWeight * CableTweak.avoidWeight).getOrElse(1.0)

    override def neighbors: Iterable[(GroundSearchState, Double)] = {
      val a = GroundSearchState(x - 1, y, gx, gy)
      val b = GroundSearchState(x + 1, y, gx, gy)
      val c = GroundSearchState(x, y - 1, gx, gy)
      val d = GroundSearchState(x, y + 1, gx, gy)
      val res = new Array[(GroundSearchState, Double)](4)
      res(0) = (a, a.weight)
      res(1) = (b, b.weight)
      res(2) = (c, c.weight)
      res(3) = (d, d.weight)
      res
    }
    override def heuristic: Double = math.abs(gx - x) + math.abs(gy - y)
    override def goal: Boolean = x == gx && y == gy
  }

  def findCablePathsForEntity(entity: Entity): Array[Array[CableNode]] = {
    val models = entity.prototype.components.collect { case m: ModelComponent => m }
    for {
      model <- models
      paths <- cableRenderSystem.getCablePathsForModel(ModelAsset(model.asset))
    } {
      return paths
    }

    DummyPaths
  }

  def scorePath(path: Array[CableNode], targetPosition: Vector3): Double = {
    val dir = (targetPosition - path.last.position).normalize
    dir dot path.last.tangentOut.normalize
  }


  def meander(from: CableNode, to: CableNode): Seq[CableNode] = {
    val begin2D = Vector2(from.position.x, from.position.z)
    val end2D = Vector2(to.position.x, to.position.z)
    val begin = cells.getCellPosition(begin2D)
    val end = cells.getCellPosition(end2D)

    val path = AStar.search(GroundSearchState(begin.x, begin.y, end.x, end.y), 10000)

    def randomOffset(): Double = (random.nextDouble() * 2.0 - 1.0) * CellSize.x * CableTweak.positionRandom

    val mids = if (path.length >= 3) {
      path.iterator.sliding(3).map(tri => {
        val Seq(a, b, c) = tri

        val ax = (a.x.toDouble + 0.5) * CellSize.x + randomOffset()
        val ay = (a.y.toDouble + 0.5) * CellSize.y + randomOffset()
        val bx = (b.x.toDouble + 0.5) * CellSize.x + randomOffset()
        val by = (b.y.toDouble + 0.5) * CellSize.y + randomOffset()
        val cx = (c.x.toDouble + 0.5) * CellSize.x + randomOffset()
        val cy = (c.y.toDouble + 0.5) * CellSize.y + randomOffset()

        val x = (ax + bx + cx) / 3.0
        val y = (ay + by + cy) / 3.0

        Vector2(x, y)
      }).toSeq
    } else {
      Seq[Vector2]()
    }

    val positions = begin2D +: mids :+ end2D

    (for (Seq(prev, cur, next) <- positions.sliding(3)) yield {
      val delta = (cur - prev).normalizeOrZero + (next - cur).normalizeOrZero
      val tan = delta * CellSize.x * CableTweak.tangentScaleMid
      val randomAngle = (random.nextDouble() * 2.0 - 1.0) * CableTweak.angleRandom
      val c = math.cos(randomAngle)
      val s = math.sin(randomAngle)

      val tx = tan.x * c + tan.y * -s
      val ty = tan.x * s + tan.y * c

      CableNode(Vector3(cur.x, 0.1, cur.y), Vector3(tx, 0.0, ty))
    }).toSeq
  }

  def adjustBlockCount(blocker: GroundBlocker, delta: Int): Unit = {
    for {
      y <- blocker.minY to blocker.maxY
      x <- blocker.minX to blocker.maxX
    } {
      val cell = cells.createCell(x, y)

      val min = Vector2.max(blocker.minWorld, cell.minWorld)
      val max = Vector2.min(blocker.maxWorld, cell.maxWorld)
      val ratio = (max.x - min.x) * (max.y - min.y) * CellInvArea
      val integerRatio = clamp((ratio * BlockMultiplier).toInt, 0, BlockMultiplier)

      cell.blockCount += delta
      cell.blockAmount += delta * integerRatio
      if (delta > 0) {
        cell.blockers :+= blocker
      } else {
        cell.blockers = cell.blockers.filter(_ != blocker)
      }
    }
  }

  def dodgeObstacles(cable: Array[CableNode]): Array[CableNode] = {
    val nodeArr = cable.toArray

    val TimeStart = 0.0
    val TimeEnd = (cable.length - 1).toDouble
    def evaluate(t: Double): Vector3 = {
      if (t <= 0.0) return nodeArr.head.position
      if (t >= TimeEnd) return nodeArr.last.position

      val index = t.toInt
      val fract = t - index.toDouble
      val prev = nodeArr(index)
      val next = nodeArr(index + 1)
      val p0 = prev.position
      val m0 = prev.tangentOut
      val p1 = next.position
      val m1 = next.tangentIn
      Hermite.interpolate(p0, m0, p1, m1, fract)
    }

    val StepSize = 0.1

    def findOffendingPosition(begin: Double, end: Double, step: Double): Option[Double] = {
      for (t <- begin to end by step) {
        val pos = evaluate(t)
        for (node <- cells.getCellContaining(pos.x, pos.z)) {
          if (!node.testPoint(Vector2(pos.x, pos.z)))
            return Some(t)
        }
      }
      None
    }

    val offsets = Array(
      Vector3(CellSize.x * +0.5, 0.0, 0.0),
      Vector3(CellSize.x * -0.5, 0.0, 0.0),
      Vector3(0.0, 0.0, CellSize.y * +0.5),
      Vector3(0.0, 0.0, CellSize.y * -0.5),
    )

    var maybeT = findOffendingPosition(TimeStart, TimeEnd, 0.1)
    var tryIx = 0
    while (maybeT.isDefined && tryIx < 2) {
      val t = maybeT.get
      tryIx += 1

      val prev = t.toInt
      val next = prev + 1

      var prevNode = nodeArr(prev)
      var nextNode = nodeArr(next)

      var isBad = true
      var moveIx = 0
      do {
        val offset = offsets(moveIx)

        nodeArr(prev) = prevNode.copy(position = prevNode.position + offset)
        nodeArr(next) = nextNode.copy(position = nextNode.position + offset)

        isBad = findOffendingPosition(prev.toDouble, next.toDouble, 0.1).isDefined
        moveIx += 1
      } while (moveIx < offsets.length && isBad)

      if (isBad) {
        println("Re-route is bad!")
        return cable
      }
    }

    nodeArr
  }

  def generateCable(cable: CableImpl): Unit = {
    val src = cable.src
    val dst = cable.dst

    val entity = new Entity(true, "Cable")

    val srcPos = src.entity.position
    val dstPos = dst.entity.position

    val srcPaths = findCablePathsForEntity(src.entity)
    val dstPaths = findCablePathsForEntity(dst.entity)

    val srcPath = srcPaths.maxBy(p => scorePath(p, dstPos - srcPos))
    val dstPath = reverseNodes(dstPaths.maxBy(p => scorePath(p, srcPos - dstPos)))

    val worldSrcPre = srcPath.map(n => n.copy(position = n.position + srcPos))
    val worldDstPre = dstPath.map(n => n.copy(position = n.position + dstPos))

    val worldSrc = worldSrcPre.dropRight(1) ++ worldSrcPre.takeRight(1).map(n => {
      n.copy(tangentOut = n.tangentOut.normalizeOrZero * CellSize.x * CableTweak.tangentScaleEndpoint)
    })
    val worldDst = worldDstPre.take(1).map(n => {
      n.copy(tangentIn = n.tangentIn.normalizeOrZero * CellSize.x * CableTweak.tangentScaleEndpoint)
    }) ++ worldDstPre.drop(1)

    val worldMid = meander(worldSrc.last, worldDst.head)
    val worldMidDodge = dodgeObstacles((worldSrc.last +: worldMid :+ worldDst.head).toArray)

    val path = worldSrc ++ worldMidDodge ++ worldDst

    val aabb = cableRenderSystem.createCable(entity, path, 0.15)

    cullingSystem.addAabb(entity, aabb, CullingSystem.MaskRender)

    entityToCable(entity) = cable
    entity.setFlag(Flag_Cable)

    cable.entityImpl = entity
    cable.nodes = path
  }

  def regenerateAllCables(): Unit = {
    val cables = entityToCable.values.toVector
    for (e <- entityToCable.keysIterator) {
      e.clearFlag(Flag_Cable)
      e.delete()
    }
    entityToCable.clear()
    for (cable <- cables) {
      generateCable(cable)
    }
  }

  override def addCable(src: Slot, dst: Slot): Cable = {
    val cable = new CableImpl(src, dst)
    generateCable(cable)
    cable
  }

  override def addGroundBlocker(entity: Entity, min: Vector2, max: Vector2): Unit = {
    val next = if (entity.hasFlag(Flag_GroundBlocker)) entityToGroundBlocker(entity) else null

    val worldMin = Vector2(entity.position.x, entity.position.z) + min
    val worldMax = Vector2(entity.position.x, entity.position.z) + max
    val cellMin = cells.getCellPosition(worldMin)
    val cellMax = cells.getCellPosition(worldMax)
    val blocker = new GroundBlocker(cellMin.x, cellMin.y, cellMax.x, cellMax.y, worldMin, worldMax, next)
    adjustBlockCount(blocker, +1)

    entity.setFlag(Flag_GroundBlocker)
    entityToGroundBlocker(entity) = blocker
  }

  override def debugDrawControlPoints(visible: EntitySet): Unit = {
    val preColor = Color.rgb(0x0000FF)
    val beginColor = Color.rgb(0xFF0000)
    val endColor = Color.rgb(0x00FF00)

    for (e <- visible.flag(Flag_Cable)) {
      val cable = entityToCable(e)

      for (node <- cable.nodes) {
        val p = node.position + Vector3(0.0, 0.3, 0.0)
        DebugDraw.drawLine(p, p - node.tangentIn, beginColor, preColor)
        DebugDraw.drawLine(p, p + node.tangentOut, beginColor, endColor)
      }

    }
  }

  override def debugDrawGroundBlockers(): Unit = {
    for (cell <- cells) {
      val min = Vector3(cell.x.toDouble * CellSize.x, 0.0, cell.y.toDouble * CellSize.y)
      val max = min + Vector3(CellSize.x - 0.05, 5.0, CellSize.y - 0.05)
      if (cell.blockCount > 0) {
        DebugDraw.drawAabb(min, max, Color.rgb(0xFF0000) * cell.moveWeight)
      } else {
        DebugDraw.drawAabb(min, max, Color.rgb(0x00FF00))
      }

      for (blocker <- cell.blockers) {
        val min = Vector3(blocker.minWorld.x, 0.0, blocker.minWorld.y)
        val max = Vector3(blocker.maxWorld.x, 5.0, blocker.maxWorld.y)
        DebugDraw.drawAabb(min, max, Color.rgb(0xFF0000))
      }
    }
  }

  override def entitiesDeleted(entities: EntitySet): Unit = {
    for (e <- entities.flag(Flag_Cable)) {
      entityToCable.remove(e)
      e.clearFlag(Flag_Cable)
    }

    for (e <- entities.flag(Flag_GroundBlocker)) {
      var blocker = entityToGroundBlocker.remove(e).get
      while (blocker != null) {
        adjustBlockCount(blocker, -1)
        blocker = blocker.next
      }
      e.clearFlag(Flag_GroundBlocker)
    }
  }

}

