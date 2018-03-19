package game.state

import core._
import asset._
import render._
import ui._
import ui.Canvas._
import platform.AppWindow
import task.Task
import LoadingState._

import scala.collection.mutable.ArrayBuffer

object LoadingState {


  private val MainFont = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")
  private val tLoading = TextStyle(MainFont, 44.0, outline = Outline(2.0))
  private val tLoadInfo = TextStyle(MainFont, 24.0, outline = Outline(1.0))

  private val lMain = 0

}

class LoadingState {

  var loadingAssets: ArrayBuffer[LoadableAsset] = null
  var numAssetsBegin = 0
  var numAssetsLeft = 0
  var frameCount = 0
  var isLoading = false
  val canvas = new Canvas()

  private val assetsRequiredForLoadingScreen = new AssetBundle(
    Font.FontShader,
    MainFont,
  )

  def start(): Unit = {
  }

  def startLoading(): Unit = {
    assetsRequiredForLoadingScreen.acquire()
    assetsRequiredForLoadingScreen.load()
    loadingAssets = AssetLoader.startLoading()
    numAssetsBegin = loadingAssets.length
    numAssetsLeft = numAssetsBegin
    isLoading = true
  }

  def stop(): Unit = {
    assetsRequiredForLoadingScreen.release()
    loadingAssets = null
  }

  def tryFinishOneAsset(): Unit = {
    var ix = 0
    while (ix < loadingAssets.length) {
      if (loadingAssets(ix) != null && loadingAssets(ix).tryFinishLoading()) {
        loadingAssets(ix) = null
        numAssetsLeft -= 1
        if (numAssetsLeft / 2 < loadingAssets.length)
          loadingAssets = loadingAssets.filter(_ != null)
        return
      }
      ix += 1
    }
  }

  def update(): Unit = {
    frameCount += 1
    AppWindow.pollEvents()

    val renderer = Renderer.get
    val frameStart = java.lang.System.nanoTime()
    var timeDeltaMs = 0

    if (isLoading) {
      do {
        Task.Main.runNextTry()
        tryFinishOneAsset()

        val time = java.lang.System.nanoTime()
        timeDeltaMs = ((time - frameStart) / 1000 / 1000).toInt
      } while (timeDeltaMs < 8)
    }


    renderer.beginFrame()
    renderer.resizeBackbuffer(AppWindow.width, AppWindow.height)
    renderer.setRenderTarget(RenderTarget.Backbuffer)
    renderer.clear(Some(Color.rgb(0x6495ED)), None)
    renderer.setBlend(true)

    if (isLoading) {
      val numLoaded = numAssetsBegin - numAssetsLeft

      val xx = 100.0
      var yy = 100.0
      yy = canvas.drawText(lMain, tLoading, xx, yy, "Loading...")
      yy = canvas.drawText(lMain, tLoadInfo, xx, yy + 5.0, s"Assets: $numLoaded/$numAssetsBegin")
      yy = canvas.drawText(lMain, tLoadInfo, xx, yy, s"Frame: $frameCount")

      canvas.render()
    }

    renderer.endFrame()

    AppWindow.swapBuffers()
  }

  def done: Boolean = numAssetsLeft == 0

}
