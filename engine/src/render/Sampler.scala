package render

import render.Sampler._

object Sampler {

  sealed abstract class Filter
  object Filter {
    case object Fixed extends Filter
    case object Nearest extends Filter
    case object Linear extends Filter
  }

  sealed abstract class Wrap
  object Wrap {
    case object Clamp extends Wrap
    case object Repeat extends Wrap
  }

  private val FFi = Filter.Fixed
  private val FNe = Filter.Nearest
  private val FLi = Filter.Linear
  private val WCl = Wrap.Clamp
  private val WRe = Wrap.Repeat

  val ClampNearest        = new Sampler(WCl, WCl, FNe, FNe, FNe, 0.0,  "ClampNearest")
  val ClampBilinear       = new Sampler(WCl, WCl, FLi, FLi, FNe, 0.0,  "ClampBilinear")
  val ClampTrilinear      = new Sampler(WCl, WCl, FLi, FLi, FLi, 0.0,  "ClampTrilinear")
  val ClampAnisotropic    = new Sampler(WCl, WCl, FLi, FLi, FLi, 16.0, "ClampAnisotropic")
  val ClampNearestNoMip   = new Sampler(WCl, WCl, FNe, FNe, FFi, 0.0,  "ClampNearestNoMip")
  val ClampBilinearNoMip  = new Sampler(WCl, WCl, FLi, FLi, FFi, 0.0,  "ClampBilinearNoMip")
  val RepeatNearest       = new Sampler(WRe, WRe, FNe, FNe, FNe, 0.0,  "RepeatNearest")
  val RepeatBilinear      = new Sampler(WRe, WRe, FLi, FLi, FNe, 0.0,  "RepeatBilinear")
  val RepeatTrilinear     = new Sampler(WRe, WRe, FLi, FLi, FLi, 0.0,  "RepeatTrilinear")
  val RepeatAnisotropic   = new Sampler(WRe, WRe, FLi, FLi, FLi, 16.0, "RepeatAnisotropic")
  val RepeatNearestNoMip  = new Sampler(WRe, WRe, FNe, FNe, FFi, 0.0,  "RepeatNearestNoMip")
  val RepeatBilinearNoMip = new Sampler(WRe, WRe, FLi, FLi, FFi, 0.0,  "RepeatBilinearNoMip")

  private var serialCounter = 0
  private def nextSerial(): Int = Sampler.synchronized {
    serialCounter += 1
    serialCounter
  }
}

class Sampler(val wrapU: Wrap, val wrapV: Wrap,
              val min: Filter, val mag: Filter, val mip: Filter,
              val maxAnisotropy: Double = 0,
              val debugName: String = "") {
  val serial = Sampler.nextSerial()
}
