package game.system.gameplay

import core._
import util._
import PathfindSystemImpl._
import ui.DebugDraw
import util.SparseGrid.CellPos

sealed trait PathfindSystem {

  /** Temporarily increase the cost of going through an area */
  def increaseDynamicWeight(min: Vector2, max: Vector2, amount: Double): Unit

  /** Find a path between two points */
  def findPath(begin: Vector2, end: Vector2): Vector[Vector2]

  /** Debug draw pathfinding grid */
  def debugDraw(fine: Boolean, coarse: Boolean): Unit

}

object PathfindSystemImpl {

  class Node(val x: Int, val y: Int) {
    var dynamicWeight: Double = 0.0

    def weight: Double = 1.0 + math.min(dynamicWeight, 2.0)
  }

  type Grid = SparseGrid[Node]

  val CoarseSize = Vector2(16.0, 16.0)
  val CoarseOffset = Vector2(0.0, 0.0)
  val FineSize = Vector2(4.0, 4.0)
  val FineOffset = Vector2(0.0, 0.0)

  /** Distance to use fine search for (in units of coarse cells) */
  val FineCutoffInCoarseCells = 3

  /** Distance to use fine search for (in world units) */
  val FineCutoffDistance = (FineCutoffInCoarseCells + 1) * CoarseSize.x

  val FineCutoffDistanceSq = FineCutoffDistance * FineCutoffDistance

  object PathfindState {
    case class Context(grid: Grid, goal: CellPos)
  }

  class PathfindState(val ctx: PathfindState.Context, val pos: CellPos) extends AStar.State[PathfindState] {

    override def equals(obj: Any): Boolean = obj match {
      case ps: PathfindState => (ps.ctx eq ctx) && ps.pos.longValue == pos.longValue
      case _ => false
    }

    override def hashCode(): IdentifierIx = System.identityHashCode(ctx) ^ pos.longValue.hashCode

    def weight: Double = {
      ctx.grid.getCell(pos) match {
        case Some(node) => node.weight
        case None => 1.0
      }
    }

    override def neighbors: Iterable[(PathfindState, Double)] = {
      val a = new PathfindState(ctx, CellPos(pos.x + 1, pos.y))
      val b = new PathfindState(ctx, CellPos(pos.x - 1, pos.y))
      val c = new PathfindState(ctx, CellPos(pos.x, pos.y + 1))
      val d = new PathfindState(ctx, CellPos(pos.x, pos.y - 1))
      val res = new Array[(PathfindState, Double)](4)
      res(0) = (a, a.weight)
      res(1) = (b, b.weight)
      res(2) = (c, c.weight)
      res(3) = (d, d.weight)
      res
    }

    override def heuristic: Double = {
      val dx = ctx.goal.x - pos.x
      val dy = ctx.goal.y - pos.y
      math.sqrt(dx*dx + dy*dy)
    }

    override def goal: Boolean = pos.longValue == ctx.goal.longValue
  }

}

final class PathfindSystemImpl extends PathfindSystem {

  val coarseGrid = new Grid(CoarseSize, CoarseOffset, (x, y) => new Node(x, y))
  val fineGrid = new Grid(FineSize, FineOffset, (x, y) => new Node(x, y))
  val allGrids = Array(coarseGrid, fineGrid)

  override def increaseDynamicWeight(min: Vector2, max: Vector2, amount: Double): Unit = {
    for (grid <- allGrids; node <- grid.createIntersecting(min, max)) {
      node.dynamicWeight += amount / grid.cellSize.x
    }
  }

  def findPathInGrid(grid: Grid, begin: Vector2, end: Vector2): Vector[Vector2] = {
    val beginCell = grid.getCellPosition(begin)
    val endCell = grid.getCellPosition(end)

    val ctx = PathfindState.Context(grid, endCell)
    val state = new PathfindState(ctx, beginCell)
    val path = AStar.search(state, 10000)

    val offset = Vector2(0.5, 0.5)
    path.toVector.map(s => grid.getPosition(s.pos, offset))
  }

  override def findPath(begin: Vector2, end: Vector2): Vector[Vector2] = {
    val distSq = begin.distanceSquaredTo(end)

    if (distSq < FineCutoffDistanceSq) {
      findPathInGrid(fineGrid, begin, end)
    } else {
      val coarsePart = findPathInGrid(coarseGrid, begin, end).dropRight(FineCutoffInCoarseCells - 1)
      val coarseEnd = coarsePart.lastOption.getOrElse(begin)
      val finePart = findPathInGrid(fineGrid, coarseEnd, end)
      coarsePart.dropRight(1) ++ finePart
    }
  }

  def debugDrawGrid(grid: Grid): Unit = {
    for (node <- grid) {
      val min = grid.getPosition(node.x, node.y, Vector2.Zero)
      val max = grid.getPosition(node.x, node.y, Vector2.One)
      DebugDraw.drawAabb(min.toXz(0.0), max.toXz(10.0), Color(node.weight * 0.2, 0.0, 0.0))
    }
  }

  def debugDraw(fine: Boolean, coarse: Boolean): Unit = {
    if (fine) debugDrawGrid(fineGrid)
    if (coarse) debugDrawGrid(coarseGrid)
  }

}

