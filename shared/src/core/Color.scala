package core

import Color._

object Color {

  def linearToSrgb(linear: Double): Double = if (linear < 0.0031308) {
    12.92 * linear
  } else {
    1.055 * math.pow(linear, 1.0 / 2.4) - 0.055
  }

  def srgbToLinear(srgb: Double): Double = if (srgb < 0.04045) {
    srgb / 12.92
  } else {
    math.pow((srgb + 0.055) / 1.055, 2.4)
  }

  /** Decompress an 8-bit sRGB color with linear alpha channel */
  def fromSrgb(r: Int, g: Int, b: Int, a: Int = 255): Color = {
    val lr = srgbToLinear(r.toDouble / 255.0)
    val lg = srgbToLinear(g.toDouble / 255.0)
    val lb = srgbToLinear(b.toDouble / 255.0)
    val la = a.toDouble / 255.0
    Color(lr, lg, lb, la)
  }

  /** Decompress an 8-bit linear color */
  def fromLinear(r: Int, g: Int, b: Int, a: Int = 255): Color = {
    val lr = r.toDouble / 255.0
    val lg = g.toDouble / 255.0
    val lb = b.toDouble / 255.0
    val la = a.toDouble / 255.0
    Color(lr, lg, lb, la)
  }

}

/**
  * A color in the _linear_ RGB colorspace. Never put straight SRGB values
  * from image sources here directly.
  *
  * The arithmetic operations operate directly on all components.
  */
case class Color(r: Double, g: Double, b: Double, a: Double = 1.0) {

  /** Compress to 8-bit sRGB with linear alpha channel */
  def toSrgb32: (Int, Int, Int, Int) = {
    val rgb = (
    clamp(linearToSrgb(r) * 255.0, 0.0, 255.0).toInt,
    clamp(linearToSrgb(g) * 255.0, 0.0, 255.0).toInt,
    clamp(linearToSrgb(b) * 255.0, 0.0, 255.0).toInt,
    clamp(a * 255.0, 0.0, 255.0).toInt)

    assert(rgb._4 < 256)

    rgb
  }

  /** Compress to 8-bit linear color */
  def toLinear32: (Int, Int, Int, Int) = (
    clamp(r * 255.0, 0.0, 255.0).toInt,
    clamp(g * 255.0, 0.0, 255.0).toInt,
    clamp(b * 255.0, 0.0, 255.0).toInt,
    clamp(a * 255.0, 0.0, 255.0).toInt)

  def *(f: Double): Color = Color(r * f, g * f, b * f, a * f)
  def /(f: Double): Color = this * (1.0 / f)
  def +(c: Color): Color = Color(r + c.r, g + c.g, b + c.b, a + c.a)
  def -(c: Color): Color = Color(r - c.r, g - c.g, b - c.b, a + c.a)
  def unary_- = Color(-r, -g, -b, -a)
}

