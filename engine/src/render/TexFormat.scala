package render

object TexFormat {

  /** 8-bit RGBA buffer */
  val Rgba = "RGBA"

  /** 8-bit sRGBA buffer */
  val SrgbA = "SRGA"

  /** 16-bit RGBA buffer */
  val Rgba16 = "Ri16"

  /** 16-bit RGB buffer */
  val Rgb16 = "Rb16"

  /** 24-bit fixed-point depth with 8-bit stencil */
  val D24S8 = "D24S"

  /** 10-bit floating point RGB buffer */
  val Rgbf10 = "Rf10"

  /** 16-bit floating point RGB buffer */
  val Rgbf16 = "Rf16"

  /** 32-bit floating point RGB buffer */
  val Rgbf32 = "Rf32"
}
