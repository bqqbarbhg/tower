package core

import java.io.ByteArrayOutputStream
import java.nio.ByteOrder

import Color._

object Color {

  val White = Color(1.0, 1.0, 1.0)
  val Black = Color(0.0, 0.0, 0.0)
  val TransparentBlack = Color(0.0, 0.0, 0.0, 0.0)

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

  /** Color from big-endian 24-bit RGB sRGB encoded color, useful with hex constants. */
  def rgb(hexRgb: Int): Color = {
    val r = (hexRgb >>> 16) & 0xFF
    val g = (hexRgb >>>  8) & 0xFF
    val b = (hexRgb >>>  0) & 0xFF
    Color.fromSrgb(r, g, b)
  }

  /** Color from big-endian 32-bit ARGB sRGB encoded color, useful with hex constants. */
  def argb(hexArgb: Int): Color = {
    val a = (hexArgb >>> 24) & 0xFF
    val r = (hexArgb >>> 16) & 0xFF
    val g = (hexArgb >>>  8) & 0xFF
    val b = (hexArgb >>>  0) & 0xFF
    Color.fromSrgb(r, g, b, a)
  }

  val littleEndian: Boolean = ByteOrder.nativeOrder == ByteOrder.LITTLE_ENDIAN

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
    clamp(linearToSrgb(r) * 255.0 + 0.5, 0.0, 255.0).toInt,
    clamp(linearToSrgb(g) * 255.0 + 0.5, 0.0, 255.0).toInt,
    clamp(linearToSrgb(b) * 255.0 + 0.5, 0.0, 255.0).toInt,
    clamp(a * 255.0, 0.0, 255.0).toInt)

    assert(rgb._4 < 256)

    rgb
  }

  /** Compress to 8-bit sRGB with linear alpha channel.
    * Packed in one integer. */
  def toSrgbInt32: Int = {
    val ir = clamp(linearToSrgb(r) * 255.0 + 0.5, 0.0, 255.0).toInt
    val ig = clamp(linearToSrgb(g) * 255.0 + 0.5, 0.0, 255.0).toInt
    val ib = clamp(linearToSrgb(b) * 255.0 + 0.5, 0.0, 255.0).toInt
    val ia = clamp(a * 255.0, 0.0, 255.0).toInt

    if (littleEndian) {
      ir | ig << 8 | ib << 16 | ia << 24
    } else {
      ia | ib << 8 | ig << 16 | ir << 24
    }
  }

  /** Compress to 8-bit linear color */
  def toLinear32: (Int, Int, Int, Int) = (
    clamp(r * 255.0 + 0.5, 0.0, 255.0).toInt,
    clamp(g * 255.0 + 0.5, 0.0, 255.0).toInt,
    clamp(b * 255.0 + 0.5, 0.0, 255.0).toInt,
    clamp(a * 255.0 + 0.5, 0.0, 255.0).toInt)

  /** Returns whether this color and `rhs` are within an epsilon of each other */
  def roughlyEqual(rhs: Color, epsilon: Double = 0.001): Boolean = (
    math.abs(r - rhs.r) <= epsilon &&
    math.abs(g - rhs.g) <= epsilon &&
    math.abs(b - rhs.b) <= epsilon &&
    math.abs(a - rhs.a) <= epsilon)

  def *(f: Double): Color = Color(r * f, g * f, b * f, a * f)
  def /(f: Double): Color = this * (1.0 / f)
  def +(c: Color): Color = Color(r + c.r, g + c.g, b + c.b, a + c.a)
  def -(c: Color): Color = Color(r - c.r, g - c.g, b - c.b, a + c.a)
  def unary_- = Color(-r, -g, -b, -a)
}

