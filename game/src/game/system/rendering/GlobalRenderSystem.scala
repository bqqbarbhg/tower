package game.system.rendering

import game.options.Options
import render._

sealed abstract class GlobalRenderSystem {

  var renderingEnabled: Boolean = true
  var msaa: Int = 0

  var screenWidth: Int = -1
  var screenHeight: Int = -1
  var mainTargetMsaa: RenderTarget = null
  var msaaResolveTarget: RenderTarget = null

  def updateScreenSize(width: Int, height: Int): Unit
  def unload(): Unit

}

final class GlobalRenderSystemImpl extends GlobalRenderSystem {


  override def updateScreenSize(width: Int, height: Int): Unit = {
    if (width == screenWidth && height == screenHeight && renderingEnabled) return

    if (width == 0 || height == 0) {
      renderingEnabled = false
    } else {
      unloadTargets()

      renderingEnabled = true
      screenWidth = width
      screenHeight = height
      Renderer.get.resizeBackbuffer(width, height)
      createTargets(width, height)
    }
  }

  def unloadTargets(): Unit = {
    if (mainTargetMsaa != null)
      mainTargetMsaa.unload()
    if (msaaResolveTarget != null)
      msaaResolveTarget.unload()
    mainTargetMsaa = null
    msaaResolveTarget = null
  }

  def createTargets(width: Int, height: Int): Unit = {
    val qOpt = Options.current.graphics.quality
    msaa = Vector(1, 2, 4, 8, 16).find(_ == qOpt.antialias).getOrElse(1)
    val format = if (qOpt.highBitdepth) TexFormat.Rgbf16 else TexFormat.Rgbf10

    val scale = math.sqrt(qOpt.resolutionFactor)
    val targetWidth = (scale * width).toInt
    val targetHeight = (scale * height).toInt

    mainTargetMsaa = RenderTarget.create(targetWidth, targetHeight, Some(format), Some(TexFormat.D24S8), false, msaa)
    msaaResolveTarget = RenderTarget.create(targetWidth, targetHeight, Some(TexFormat.Rgba), None, false)
  }

  override def unload(): Unit = {
    unloadTargets()
    screenWidth = -1
    screenHeight = -1
  }

}

