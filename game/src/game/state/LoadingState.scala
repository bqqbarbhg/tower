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

  lazy val assetBundle = {
    val bundle = new AssetBundle(
      Font.FontShader,
      MainFont,
    )

    bundle.acquire()
    bundle
  }

}

class LoadingState extends GameState {

  var loadingAssets: ArrayBuffer[LoadableAsset] = null
  var numAssetsBegin = 0
  var numAssetsLeft = 0
  var frameCount = 0
  var isLoading = false
  val canvas = new Canvas()

  override def load(): Unit = {
    LoadingState.assetBundle.load()
  }

  override def start(): Unit = {
    loadingAssets = AssetLoader.startLoading()
    numAssetsBegin = loadingAssets.length
    numAssetsLeft = numAssetsBegin
    isLoading = true
  }

  override def stop(): Unit = {
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

  override def update(): Unit = {
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

  override def done: Boolean = numAssetsLeft == 0 && isLoading

}
