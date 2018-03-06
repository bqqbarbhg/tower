package core

/**
  * Utilities for performing hermite interpolation.
  */
object Hermite {

  def interpolate(p0: Float, m0: Float, p1: Float, m1: Float, t: Float): Float = {
    val t2 = t*t
    val t3 = t2*t
    val h00 = 2*t3 - 3*t2 + 1
    val h10 = t3 - 2*t2 + t
    val h01 = -2*t3 + 3*t2
    val h11 = t3 - t2
    h00*p0 + h10*m0 + h01*p1 + h11*m1
  }

  def interpolate(p0: Double, m0: Double, p1: Double, m1: Double, t: Double): Double = {
    val t2 = t*t
    val t3 = t2*t
    val h00 = 2*t3 - 3*t2 + 1
    val h10 = t3 - 2*t2 + t
    val h01 = -2*t3 + 3*t2
    val h11 = t3 - t2
    h00*p0 + h10*m0 + h01*p1 + h11*m1
  }

}
