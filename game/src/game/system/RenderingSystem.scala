package game.system

import game.options.Options
import render._

object RenderingSystem {

  var renderingEnabled: Boolean = true
  var msaa: Int = 0

  var screenWidth: Int = -1
  var screenHeight: Int = -1
  var MainTargetMsaa: RenderTarget = null

  def updateScreenSize(width: Int, height: Int): Unit = {
    if (width == screenWidth && height == screenHeight) return

    if (width == 0 || height == 0) {
      renderingEnabled = false
    } else {
      renderingEnabled = true
      screenWidth = width
      screenHeight = height
      Renderer.get.resizeBackbuffer(width, height)
      createTargets(width, height)
    }
  }

  def createTargets(width: Int, height: Int): Unit = {

    val qOpt = Options.current.graphics.quality
    msaa = Vector(1, 2, 4, 8, 16).find(_ == qOpt.antialias).getOrElse(1)
    val format = if (qOpt.highBitdepth) TexFormat.Rgbf16 else TexFormat.Rgbf10

    MainTargetMsaa = RenderTarget.create(width, height, Some(format), Some(TexFormat.D24S8), false, msaa)
  }

  def unload(): Unit = {
    screenWidth = -1
    screenHeight = -1
    MainTargetMsaa.unload()
    MainTargetMsaa = null
  }

}

