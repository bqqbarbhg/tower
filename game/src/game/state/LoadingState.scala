package game.state

import core._
import asset._
import render._
import platform.AppWindow
import task.Task
import ui.Font
import ui.Font.TextDraw

import scala.collection.mutable.ArrayBuffer

class LoadingState {

  var loadingAssets: ArrayBuffer[LoadableAsset] = null
  var numAssetsBegin = 0
  var numAssetsLeft = 0
  var frameCount = 0

  private val fontAsset = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")
  private val assetsRequiredForLoadingScreen = new AssetBundle(
    Font.FontShader,
    fontAsset,
  )

  def start(): Unit = {
    assetsRequiredForLoadingScreen.acquire()
    assetsRequiredForLoadingScreen.load()
    loadingAssets = AssetLoader.startLoading()
    numAssetsBegin = loadingAssets.length
    numAssetsLeft = numAssetsBegin
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

    val frameStart = java.lang.System.nanoTime()
    var timeDeltaMs = 0

    do {
      Task.Main.runNextTry()
      tryFinishOneAsset()

      val time = java.lang.System.nanoTime()
      timeDeltaMs = ((time - frameStart) / 1000 / 1000).toInt
    } while (timeDeltaMs < 8)

    val renderer = Renderer.get

    renderer.beginFrame()
    renderer.resizeBackbuffer(AppWindow.width, AppWindow.height)
    renderer.setRenderTarget(RenderTarget.Backbuffer)
    renderer.clear(Some(Color.rgb(0x6495ED)), None)
    renderer.setBlend(true)

    val font = fontAsset.get

    val numLoaded = numAssetsBegin - numAssetsLeft

    val draws = ArrayBuffer[TextDraw]()

    {
      val text = s"Loading: $numLoaded/$numAssetsBegin"
      draws += TextDraw(text, 0, text.length, Vector2(80.0, 80.0), 32.0, Color.Black, 2.0, 0)
      draws += TextDraw(text, 0, text.length, Vector2(80.0, 80.0), 32.0, Color.White, 0.0, 1)
    }

    {
      val text = s"Frame: $frameCount"
      draws += TextDraw(text, 0, text.length, Vector2(80.0, 110.0), 24.0, Color.Black, 2.0, 0)
      draws += TextDraw(text, 0, text.length, Vector2(80.0, 110.0), 24.0, Color.White, 0.0, 1)
    }

    font.render(draws)
    renderer.endFrame()

    AppWindow.swapBuffers()
  }

  def done: Boolean = numAssetsLeft == 0

}
