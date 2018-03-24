package game.system

import render._

object RenderingSystem {

  var renderingEnabled: Boolean = true

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
    MainTargetMsaa = RenderTarget.create(width, height, Some("SRGB"), Some("D24S"), false, 8)
  }

  def unload(): Unit = {
    screenWidth = -1
    screenHeight = -1
    MainTargetMsaa.unload()
    MainTargetMsaa = null
  }

}

