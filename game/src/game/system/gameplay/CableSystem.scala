package game.system.gameplay

import core._
import asset.ModelAsset
import game.component.{BuildableComponent, ModelComponent}
import game.system._
import game.system.Entity._
import game.system.base._
import game.system.rendering._
import game.system.gameplay.TowerSystem.Slot
import game.system.rendering.CableRenderSystem.{CableMesh, CableNode, ModelCablePath}
import CableSystem._
import CableSystemImpl._
import io.property._
import ui.DebugDraw
import util.SparseGrid.CellPos
import util._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object CableSystem {

  sealed abstract class Cable {
    def entity: Entity

    def setPulse(relativePos: Double): Unit
    def clearPulse(): Unit
  }

  object CableTweak extends PropertyContainer {
    private val arr = MacroPropertySet.make[CableTweak.type]()
    override val propertySet: PropertySet = new PropertySet("CableTweak", arr) {
      range("tangentScaleMid", 0.0, 1.0)
      range("tangentScaleEndpoint", 0.0, 1.0)
    }

    var tangentScaleMid: DoubleProp.Type = 0.5
    var tangentScaleEndpoint: DoubleProp.Type = 0.5
    var stepSize: DoubleProp.Type = 1.0
    var tangentGainOnRemove: DoubleProp.Type = 1.5
    var surroundingBlockerWeight: DoubleProp.Type = 1.0
    var regenerateCables: BoolProp.Type = false
  }

}

sealed trait CableSystem extends EntityDeleteListener {

  /** Add a new cable */
  def addCable(src: Slot, dst: Slot): Cable

  /** Remove a cable between slots */
  def removeCable(src: Slot, dst: Slot): Unit

  /** Block cables from going through an object */
  def addGroundBlocker(entity: Entity, min: Vector2, max: Vector2): Unit

  /** Generate queued cables */
  def generateCables(): Unit

  /** Render debug view of cable control points */
  def debugDrawControlPoints(visible: EntitySet)

  /** Render debug view of ground blockers */
  def debugDrawGroundBlockers()

  /** Regenerate all the cables */
  def regenerateAllCables(): Unit

}

object CableSystemImpl {

  val NoCables = Array[CableImpl]()
  val NoCells = Array[GroundCell]()

  final class CableImpl(val src: Slot, val dst: Slot) extends Cable {
    var entityImpl: Entity = null
    var nodes: Array[CableNode] = _
    var cells: Array[GroundCell] = NoCells
    var mesh: CableMesh = null
    var needsToBeGenerated: Boolean = false
    var deleted: Boolean = false

    override def entity: Entity = entityImpl

    override def setPulse(relativePos: Double): Unit = {
      if (mesh != null) {
        mesh.pulseSize = 15.0
        mesh.pulsePosition = relativePos * (mesh.length + mesh.pulseSize)
      }
    }

    override def clearPulse(): Unit = {
      if (mesh != null) {
        mesh.pulsePosition = 0.0
        mesh.pulseSize = 0.0
      }
    }
  }

  val NoPaths = Array(ModelCablePath("", Array[CableNode]()))
  val DummyPaths = Array(Array(CableNode(Vector3.Zero, Vector3.Zero)))

  def reverseNodes(nodes: Array[CableNode]): Array[CableNode] = {
    nodes.reverse.map(n => n.copy(tangentIn = -n.tangentIn, tangentOut = -n.tangentOut))
  }

  val CellOffset = Vector2(2.0, 2.0)
  val CellSize = Vector2(8.0, 8.0)
  val CellArea = CellSize.x * CellSize.y
  val CellInvArea = 1.0 / CellArea
  val InvCellSize = Vector2(1.0 / CellSize.x, 1.0 / CellSize.y)

  val BlockMultiplier = 10000
  val InvBlockMultiplier = 1.0 / BlockMultiplier.toDouble

  def worldToCell(x: Double, y: Double): CellPos = {
    val ix = math.floor((x - CellOffset.x) * InvCellSize.x).toInt
    val iy = math.floor((y - CellOffset.y) * InvCellSize.y).toInt
    CellPos(ix, iy)
  }
  def worldToCell(pos: Vector2): CellPos = worldToCell(pos.x, pos.y)
  def worldToCell(pos: Vector3): CellPos = worldToCell(pos.x, pos.z)

  def cellToWorld(x: Int, y: Int): Vector2 = {
    val fx = x * CellSize.x + CellOffset.x
    val fy = y * CellSize.y + CellOffset.y
    Vector2(fx, fy)
  }
  def cellToWorld(pos: CellPos): Vector2 = cellToWorld(pos.x, pos.y)


  class GroundCell(val x: Int, val y: Int) {
    var blockCount: Int = 0
    var blockAmount: Int = 0
    val minWorld = Vector2(x * CellSize.x, y * CellSize.y) + CellOffset
    val maxWorld = Vector2((x + 1) * CellSize.x, (y + 1) * CellSize.y) + CellOffset
    var cables: Array[CableImpl] = NoCables

    var blockers = new Array[GroundBlocker](0)

    def moveWeight: Double = blockAmount.toDouble * InvBlockMultiplier

    def testPoint(x: Double, y: Double, ignoredEntities: Iterable[Entity] = None): Boolean = {
      for (blocker <- blockers) {
        if (!ignoredEntities.exists(_ == blocker.entity)) {
          val min = blocker.minWorld
          val max = blocker.maxWorld
          if (x >= min.x && y >= min.y && x <= max.x && y <= max.y)
            return false
        }
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
        var t1 = (blocker.minWorld.x - a.x) * idx
        var t2 = (blocker.maxWorld.x - a.x) * idx

        var tMin = math.min(t1, t2)
        var tMax = math.max(t1, t2)

        t1 = (blocker.minWorld.y - a.y) * idy
        t2 = (blocker.maxWorld.y - a.y) * idy

        tMin = math.max(tMin, math.min(t1, t2))
        tMax = math.min(tMax, math.max(t1, t2))

        if (tMin <= tMax && tMax >= 0.0 && tMin <= diffLen) {
          return false
        }
      }

      true
    }
  }

  class GroundBlocker(val entity: Entity, val minX: Int, val minY: Int, val maxX: Int, val maxY: Int, val minWorld: Vector2, val maxWorld: Vector2, val next: GroundBlocker) {
  }

  def extraWeightToDistance(link: Vector3, x: Int, y: Int): Double = {
    val pos = Vector3(x.toDouble * CellSize.x * 0.5 + CellOffset.x, 0.0, y.toDouble * CellSize.y * 0.5 + CellOffset.y)
    val near = clamp(0.0, 1.0, pos.distanceSquaredTo(link) * 0.1) * 10.0
    val far = clamp(0.0, 1.0, pos.distanceTo(link) * 0.1) * 5.0
    near + far
  }

}

final class CableSystemImpl extends CableSystem {
  val slotToCable = new mutable.HashMap[Slot, CableImpl]()
  val entityToCable = new mutable.HashMap[Entity, CableImpl]()
  val entityToGroundBlocker = new mutable.HashMap[Entity, GroundBlocker]()
  val random = new Random()
  val cells = new SparseGrid[GroundCell](CellSize, CellOffset, (x, y) => new GroundCell(x, y))
  val cablesToGenerate = new ArrayBuffer[CableImpl]()

  case class GroundSearchState(x: Int, y: Int, gxMin: Int, gxMax: Int, gyMin: Int, gyMax: Int, goalEntity: Entity, goalLink: Vector3) extends AStar.State[GroundSearchState] {
    val pos = Vector2(x * CellSize.x * 0.5 + CellOffset.x, y * CellSize.y * 0.5 + CellOffset.y)
    val cell = cells.getCell(Math.floorDiv(x, 2), Math.floorDiv(y, 2))

    def weight(prevPos: Vector2, prevCell: Option[GroundCell]): Double = {
      if (prevCell != cell) {
        for (c <- prevCell) {
          if (!c.testLineSegment(prevPos, pos)) return 100.0
        }
      }

      var score = cell match {
        case Some(c) =>
          if (!c.testLineSegment(prevPos, pos)) return 100.0
          1.0 + c.moveWeight * CableTweak.surroundingBlockerWeight
        case None => 1.0
      }

      if (goal) {
        score += extraWeightToDistance(goalLink, x, y)
      }

      score
    }

    override def neighbors: Iterable[(GroundSearchState, Double)] = {
      val a = GroundSearchState(x - 1, y, gxMin, gxMax, gyMin, gyMax, goalEntity, goalLink)
      val b = GroundSearchState(x + 1, y, gxMin, gxMax, gyMin, gyMax, goalEntity, goalLink)
      val c = GroundSearchState(x, y - 1, gxMin, gxMax, gyMin, gyMax, goalEntity, goalLink)
      val d = GroundSearchState(x, y + 1, gxMin, gxMax, gyMin, gyMax, goalEntity, goalLink)
      val res = new Array[(GroundSearchState, Double)](4)
      res(0) = (a, a.weight(pos, cell))
      res(1) = (b, b.weight(pos, cell))
      res(2) = (c, c.weight(pos, cell))
      res(3) = (d, d.weight(pos, cell))
      res
    }
    override def heuristic: Double = {
      val gx = clamp(x, gxMin, gxMax)
      val gy = clamp(y, gyMin, gyMax)
      val dx = gx - x
      val dy = gy - y
      math.sqrt(dx*dx + dy*dy)
    }
    override def goal: Boolean = x >= gxMin && y >= gyMin && x <= gxMax && y <= gyMax
  }

  def findCablePathsForEntity(entity: Entity): Array[ModelCablePath] = {
    val models = entity.prototype.components.collect { case m: ModelComponent => m }
    for {
      model <- models
      paths <- cableRenderSystem.getCablePathsForModel(ModelAsset(model.asset))
    } {
      return paths
    }

    NoPaths
  }

  def scorePath(path: Array[CableNode], targetPosition: Vector3): Double = {
    targetPosition.distanceSquaredTo(path.last.position)
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
        for (cable <- cell.cables)
          queueGeneration(cable)
      } else {
        cell.blockers = cell.blockers.filter(_ != blocker)
      }
    }
  }

  def layoutCable(from: Vector3, to: Vector3, fromLink: Vector3, toLink: Vector3, fromC: BuildableComponent, toC: BuildableComponent, goal: Entity, cable: CableImpl): Seq[CableNode] = {

    val begin2D = Vector2(from.x, from.z) - CellOffset + fromC.cableOffset
    val end2D = Vector2(to.x, to.z) - CellOffset + toC.cableOffset

    val bx = math.round(begin2D.x / CellSize.x * 2.0).toInt
    val by = math.round(begin2D.y / CellSize.y * 2.0).toInt
    val ex = math.round(end2D.x / CellSize.x * 2.0).toInt
    val ey = math.round(end2D.y / CellSize.y * 2.0).toInt

    val bxMin = bx + fromC.cableMinX
    val byMin = by + fromC.cableMinY
    val bxMax = bx + fromC.cableMaxX
    val byMax = by + fromC.cableMaxY
    val exMin = ex + toC.cableMinX
    val eyMin = ey + toC.cableMinY
    val exMax = ex + toC.cableMaxX
    val eyMax = ey + toC.cableMaxY

    val starts = new ArrayBuffer[(GroundSearchState, Double)]()

    def startState(x: Int, y: Int): (GroundSearchState, Double) = {
      val extra = extraWeightToDistance(fromLink, x, y)
      (GroundSearchState(x, y, exMin, exMax, eyMin, eyMax, goal, toLink), extra)
    }

    for (x <- bxMin to bxMax) {
      starts += startState(x, byMin)
      starts += startState(x, byMax)
    }
    for (y <- (byMin + 1) to (byMax - 1)) {
      starts += startState(bxMin, y)
      starts += startState(bxMax, y)
    }

    val path = AStar.search(starts, 10000)

    var dropStart = path.indexWhere(p => p.x < bxMin || p.x > bxMax || p.y < byMin || p.y > byMax)
    var dropEnd = path.reverseIterator.indexWhere(p => p.x < exMin || p.x > exMax || p.y < eyMin || p.y > eyMax)

    if (dropStart <= 0) dropStart = 1
    if (dropEnd <= 0) dropEnd = 1
    dropStart -= 1
    dropEnd -= 1

    val trimmed = path.drop(dropStart).dropRight(dropEnd)

    val points = for (node <- trimmed) yield {
      val px = node.x * CellSize.x * 0.5 + CellOffset.x
      val py = node.y * CellSize.y * 0.5 + CellOffset.y
      Vector3(px, 0.1, py)
    }

    var nodes = (for (i <- points.indices) yield {
      val prev = points.lift(i - 1).getOrElse(from)
      val cur = points(i)
      val next = points.lift(i + 1).getOrElse(to)

      val dir = (cur - prev) + (next - cur)
      CableNode(cur, dir * CableTweak.tangentScaleMid)
    }).toBuffer

    def evaluateSpan(a: CableNode, b: CableNode): Boolean = {
      val step = math.max(CableTweak.stepSize / a.position.distanceTo(b.position), 0.02)
      for (t <- 0.0 to 1.001 by step) {
        val pos = CableRenderSystem.evaluate(a, b, t)
        for (cell <- cells.getCellContaining(pos.x, pos.z)) {
          if (!cell.testPoint(pos.x, pos.z)) return false
        }
      }
      true
    }

    var prevCell: GroundCell = null

    var cellList = new ArrayBuffer[GroundCell]()

    def markSpan(a: CableNode, b: CableNode): Unit = {
      val step = math.max(CableTweak.stepSize / a.position.distanceTo(b.position), 0.02)
      for (t <- 0.0 to 1.001 by step) {
        val pos = CableRenderSystem.evaluate(a, b, t)
        val cell = cells.createCellContaining(pos.x, pos.z)

        if (cell != prevCell) {
          prevCell = cell
          if (!cell.cables.contains(cable)) {
            cell.cables :+= cable
            cellList += cell
          }
        }
      }
    }

    def removeUnncesessaryNodes(): Unit = {

      nodes = (for (i <- nodes.indices) yield {
        val prev = nodes.lift(i - 1).map(_.position).getOrElse(from)
        val cur = nodes(i).position
        val next = nodes.lift(i + 1).map(_.position).getOrElse(to)

        val dir = (cur - prev) + (next - cur)
        CableNode(cur, dir * CableTweak.tangentScaleMid)
      }).toBuffer

      var index = 1
      while (index < nodes.length - 1) {
        val a = nodes(index - 1)
        val b = nodes(index)
        val c = nodes(index + 1)

        if (evaluateSpan(a, c)) {
          nodes(index) = null
          index += 2
        } else {
          index += 1
        }
      }

      nodes = nodes.filter(_ != null)

    }

    for (i <- 0 until 2) {
      removeUnncesessaryNodes()
    }

    {
      var index = 0
      while (index < nodes.length - 1) {
        val a = nodes(index)
        val b = nodes(index + 1)
        markSpan(a, b)
        index += 1
      }
    }

    cable.cells = cellList.toArray

    nodes.toSeq
  }

  def generateCable(cable: CableImpl): Unit = {
    val src = cable.src
    val dst = cable.dst

    val entity = new Entity(true, "Cable")

    val worldPointToSrc = src.entity.inverseTransformPoint _
    val worldPointToDst = dst.entity.inverseTransformPoint _
    val srcPointToWorld = src.entity.transformPoint _
    val dstPointToWorld = dst.entity.transformPoint _
    val srcDirToWorld = src.entity.transformDirection _
    val dstDirToWorld = dst.entity.transformDirection _

    val srcPos = src.entity.position
    val dstPos = dst.entity.position

    val srcModelPaths = findCablePathsForEntity(src.entity).filter(_.name.startsWith(src.info.cableNode))
    val dstModelPaths = findCablePathsForEntity(dst.entity).filter(_.name.startsWith(dst.info.cableNode))

    val srcPaths = if (srcModelPaths.nonEmpty) srcModelPaths.map(_.nodes) else DummyPaths
    val dstPaths = if (dstModelPaths.nonEmpty) dstModelPaths.map(_.nodes) else DummyPaths

    val srcBuildable = src.entity.prototype.find(BuildableComponent).get
    val dstBuildable = dst.entity.prototype.find(BuildableComponent).get

    val srcLink = srcPointToWorld(srcPaths.head.head.position)
    val dstLink = dstPointToWorld(dstPaths.head.head.position)

    val worldMid = layoutCable(srcPos, dstPos, srcLink, dstLink, srcBuildable, dstBuildable, dst.entity, cable)

    val (srcPath, dstPath) = if (worldMid.nonEmpty) {
      val srcPath = srcPaths.minBy(p => scorePath(p, worldPointToSrc(worldMid.head.position)))
      val dstPath = reverseNodes(dstPaths.minBy(p => scorePath(p, worldPointToDst(worldMid.last.position))))
      (srcPath, dstPath)
    } else {
      val srcPath = srcPaths.minBy(p => scorePath(p, worldPointToSrc(dstPos)))
      val dstPath = reverseNodes(dstPaths.minBy(p => scorePath(p, worldPointToDst(srcPos))))
      (srcPath, dstPath)
    }


    val worldSrc = srcPath.map(n => n.copy(position = srcPointToWorld(n.position),
      tangentIn = srcDirToWorld(n.tangentIn), tangentOut = srcDirToWorld(n.tangentOut)))
    val worldDst = dstPath.map(n => n.copy(position = dstPointToWorld(n.position),
      tangentIn = dstDirToWorld(n.tangentIn), tangentOut = dstDirToWorld(n.tangentOut)))

    val path = worldSrc ++ worldMid ++ worldDst

    val cableMesh = cableRenderSystem.createCable(entity, path, 0.2)

    cullingSystem.addAabb(entity, cableMesh.aabb, CullingSystem.MaskRender)

    entityToCable(entity) = cable
    entity.setFlag(Flag_Cable)

    cable.mesh = cableMesh
    cable.entityImpl = entity
    cable.nodes = path
  }

  override def generateCables(): Unit = {
    for (cable <- cablesToGenerate) {
      if (!cable.deleted) {
        cable.needsToBeGenerated = false

        if (cable.entityImpl != null) {
          removeCable(cable)
          cable.entityImpl.clearFlag(Flag_Cable)
          cable.entityImpl.delete()
        }
        generateCable(cable)
      }
    }

    cablesToGenerate.clear()
  }

  def queueGeneration(cable: CableImpl): Unit = {
    if (cable.needsToBeGenerated) return
    cable.needsToBeGenerated = true
    cablesToGenerate += cable
  }

  def regenerateAllCables(): Unit = {
    for (cable <- entityToCable.valuesIterator) {
      queueGeneration(cable)
    }
  }

  def removeCable(cable: CableImpl): Unit = {
    for (cell <- cable.cells) {
      cell.cables = cell.cables.filterNot(_ eq cable)
    }
  }

  override def removeCable(src: Slot, dst: Slot): Unit = {
    for (cable <- slotToCable.remove(src)) {
      if (cable.entity != null && !cable.deleted) {
        cable.deleted = true
        cable.entity.delete()
      }
    }

    for (cable <- slotToCable.remove(dst)) {
      if (cable.entity != null && !cable.deleted) {
        cable.deleted = true
        cable.entity.delete()
      }
    }
  }

  override def addCable(src: Slot, dst: Slot): Cable = {
    removeCable(src, dst)

    val cable = new CableImpl(src, dst)
    queueGeneration(cable)
    slotToCable(src) = cable
    slotToCable(dst) = cable
    cable
  }

  override def addGroundBlocker(entity: Entity, min: Vector2, max: Vector2): Unit = {
    val next = if (entity.hasFlag(Flag_GroundBlocker)) entityToGroundBlocker(entity) else null

    val worldA = entity.transformPoint(Vector3(min.x, 0.0, min.y))
    val worldB = entity.transformPoint(Vector3(max.x, 0.0, max.y))
    val worldMin3D = Vector3.min(worldA, worldB)
    val worldMax3D = Vector3.max(worldA, worldB)
    val worldMin = Vector2(worldMin3D.x, worldMin3D.z)
    val worldMax = Vector2(worldMax3D.x, worldMax3D.z)
    val cellMin = cells.getCellPosition(worldMin)
    val cellMax = cells.getCellPosition(worldMax)
    val blocker = new GroundBlocker(entity, cellMin.x, cellMin.y, cellMax.x, cellMax.y, worldMin, worldMax, next)
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

    val c = cells.createCellContaining(Vector2(16.0, 8.0))
    c.testLineSegment(Vector2(16.0 -3.0, 8.0), Vector2(16.0 +3.0, 8.0))

    for (cell <- cells) {
      val min2D = cellToWorld(cell.x, cell.y)
      val min = Vector3(min2D.x, 0.0, min2D.y)
      val max = min + Vector3(CellSize.x - 0.05, 5.0, CellSize.y - 0.05)

      if (cell.blockCount > 0) {
        DebugDraw.drawAabb(min, max, Color.rgb(0xFF0000) * cell.moveWeight)
      } else {
        DebugDraw.drawAabb(min, max, Color.rgb(0x00FF00))
      }

      if (cell.cables.nonEmpty) {
        DebugDraw.drawAabb(min + Vector3(0.05, 0.05, 0.05), max - Vector3(0.05, 0.05, 0.05), Color.rgb(0xFFFFFF))
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
      val cable = entityToCable.remove(e).get
      removeCable(cable)
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

