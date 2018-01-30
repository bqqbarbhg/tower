package tower

package object math {

  def clamp(v: Int, min: Int, max: Int) = scala.math.min(scala.math.max(v, min), max)
  def clamp(v: Float, min: Float, max: Float) = scala.math.min(scala.math.max(v, min), max)
  def clamp(v: Double, min: Double, max: Double) = scala.math.min(scala.math.max(v, min), max)

}
