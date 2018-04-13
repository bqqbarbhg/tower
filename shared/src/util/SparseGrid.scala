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

}

/** Spatial 2D sparse grid structure comprised of cells of some size. */
class SparseGrid[A](val cellSize: Vector2, val constructor: (Int, Int) => A) extends Iterable[A] {
  val mapping = new mutable.HashMap[Long, A]()

  def createCell(x: Int, y: Int): A = {
    val key = CellPos(x, y).longValue
    mapping.getOrElseUpdate(key, constructor(x, y))
  }

  def getCell(x: Int, y: Int): Option[A] = {
    val key = CellPos(x, y).longValue
    mapping.get(key)
  }

  def createCellContaining(x: Double, y: Double): A = {
    val cx = math.floor(x / cellSize.x).toInt
    val cy = math.floor(y / cellSize.y).toInt
    createCell(cx, cy)
  }

  def createCellContaining(position: Vector2): A = createCellContaining(position.x, position.y)

  def getCellContaining(x: Double, y: Double): Option[A] = {
    val cx = math.floor(x / cellSize.x).toInt
    val cy = math.floor(y / cellSize.y).toInt
    getCell(cx, cy)
  }

  def getCellContaining(position: Vector2): Option[A] = getCellContaining(position.x, position.y)

  def getCellPosition(x: Double, y: Double): CellPos = {
    val cx = math.floor(x / cellSize.x).toInt
    val cy = math.floor(y / cellSize.y).toInt
    CellPos(cx, cy)
  }
  def getCellPosition(position: Vector2): CellPos = getCellPosition(position.x, position.y)

  override def iterator = mapping.valuesIterator
}

