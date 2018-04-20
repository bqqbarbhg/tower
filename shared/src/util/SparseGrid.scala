package util

import scala.collection.mutable

import core.Vector2
import SparseGrid._

object SparseGrid {

  object CellPos {
    def apply(x: Int, y: Int): CellPos = {
      val key: Long = (x.toLong & 0xFFFFFFFFL) | (y.toLong << 32L)
      new CellPos(key)
    }
  }

  class CellPos(val longValue: Long) extends AnyVal {

    def x: Int = (longValue & 0xFFFFFFFFL).toInt
    def y: Int = ((longValue >>> 32L) & 0xFFFFFFFFL).toInt
  }

  class RectCreateIterator[A](val grid: SparseGrid[A], val min: CellPos, val max: CellPos) extends Iterator[A] {
    private var x: Int = min.x
    private var y: Int = min.y
    private var res: Option[A] = None

    private def advance(): Unit = {
      if (x <= max.x && y <= max.y) {
        res = Some(grid.createCell(x, y))
        if (x == max.x) {
          y += 1
          x = min.x
        } else {
          x += 1
        }
      } else {
        res = None
      }
    }

    advance()

    override def hasNext: Boolean = res.isDefined
    override def next(): A = {
      require(res.isDefined, "next() on empty iterator")
      val r = res.get
      advance()
      r
    }
  }

  class RectGetIterator[A](val grid: SparseGrid[A], val min: CellPos, val max: CellPos) extends Iterator[A] {
    private var x: Int = min.x
    private var y: Int = min.y
    private var res: Option[A] = None

    private def advance(): Unit = {
      res = None
      while ((x <= max.x && y <= max.y) && res.isEmpty) {
        res = grid.getCell(x, y)
        if (x == max.x) {
          y += 1
          x = min.x
        } else {
          x += 1
        }
      }
    }

    advance()

    override def hasNext: Boolean = res.isDefined
    override def next(): A = {
      require(res.isDefined, "next() on empty iterator")
      val r = res.get
      advance()
      r
    }
  }

}

/** Spatial 2D sparse grid structure comprised of cells of some size. */
class SparseGrid[A](val cellSize: Vector2, val cellOffset: Vector2, val constructor: (Int, Int) => A) extends Iterable[A] {
  val mapping = new mutable.HashMap[Long, A]()

  def createCell(x: Int, y: Int): A = {
    val key = CellPos(x, y).longValue
    mapping.getOrElseUpdate(key, constructor(x, y))
  }

  def getCell(pos: CellPos): Option[A] = mapping.get(pos.longValue)

  def getCell(x: Int, y: Int): Option[A] = {
    val key = CellPos(x, y).longValue
    mapping.get(key)
  }

  def createCellContaining(x: Double, y: Double): A = {
    val cx = math.floor((x - cellOffset.x) / cellSize.x).toInt
    val cy = math.floor((y - cellOffset.y) / cellSize.y).toInt
    createCell(cx, cy)
  }

  def createCellContaining(position: Vector2): A = createCellContaining(position.x, position.y)

  def getCellContaining(x: Double, y: Double): Option[A] = {
    val cx = math.floor((x - cellOffset.x) / cellSize.x).toInt
    val cy = math.floor((y - cellOffset.y) / cellSize.y).toInt
    getCell(cx, cy)
  }

  def getCellContaining(position: Vector2): Option[A] = getCellContaining(position.x, position.y)

  def getCellPosition(x: Double, y: Double): CellPos = {
    val cx = math.floor((x - cellOffset.x) / cellSize.x).toInt
    val cy = math.floor((y - cellOffset.y) / cellSize.y).toInt
    CellPos(cx, cy)
  }
  def getCellPosition(position: Vector2): CellPos = getCellPosition(position.x, position.y)

  def createRange(minX: Int, minY: Int, maxX: Int, maxY: Int): Iterator[A] = createRange(CellPos(minX, minY), CellPos(maxX, maxY))
  def createRange(minPos: CellPos, maxPos: CellPos): Iterator[A] = {
    new RectCreateIterator[A](this, minPos, maxPos)
  }

  def createIntersecting(min: Vector2, max: Vector2): Iterator[A] = createIntersecting(min.x, min.y, max.x, max.y)
  def createIntersecting(minX: Double, minY: Double, maxX: Double, maxY: Double): Iterator[A] = {
    val minPos = getCellPosition(minX, minY)
    val maxPos = getCellPosition(maxX, maxY)
    new RectCreateIterator[A](this, minPos, maxPos)
  }

  def getRange(minX: Int, minY: Int, maxX: Int, maxY: Int): Iterator[A] = createRange(CellPos(minX, minY), CellPos(maxX, maxY))
  def getRange(minPos: CellPos, maxPos: CellPos): Iterator[A] = {
    new RectGetIterator[A](this, minPos, maxPos)
  }

  def getIntersecting(min: Vector2, max: Vector2): Iterator[A] = getIntersecting(min.x, min.y, max.x, max.y)
  def getIntersecting(minX: Double, minY: Double, maxX: Double, maxY: Double): Iterator[A] = {
    val minPos = getCellPosition(minX, minY)
    val maxPos = getCellPosition(maxX, maxY)
    new RectGetIterator[A](this, minPos, maxPos)
  }

  def getPosition(cell: CellPos, offset: Vector2): Vector2 = getPosition(cell.x, cell.y, offset)
  def getPosition(x: Int, y: Int, offset: Vector2): Vector2 = {
    val xx = cellOffset.x + (x.toDouble + offset.x) * cellSize.x
    val yy = cellOffset.y + (y.toDouble + offset.y) * cellSize.y
    Vector2(xx, yy)
  }

  override def iterator = mapping.valuesIterator
}

