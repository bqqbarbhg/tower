package game.state

import core._
import asset._
import render._
import ui._
import ui.Canvas._
import platform.AppWindow
import task.Task
import LoadingState._
import game.system._

import scala.collection.mutable.ArrayBuffer

object LoadingState {


  private val LoadingAtlas = AtlasAsset("atlas/loading.s2at")
  private val MainFont = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")
  private val tLoading = TextStyle(MainFont, 44.0)
  private val tLoadInfo = TextStyle(MainFont, 24.0)

  val TurretSpinnerFill = Array.tabulate(8)(i => Identifier(s"loading/sprite/loading_turret_anim_fill.png.$i"))
  val TurretSpinnerOutline = Array.tabulate(8)(i => Identifier(s"loading/sprite/loading_turret_anim_outline.png.$i"))
  val Background = Identifier("loading/sprite/background.png")

  private val lMain = 0
  private val lSpinner = 1

  lazy val assetBundle = {
    val bundle = new AssetBundle(
      "LoadingState",
      Font.FontShader,
      SpriteBatch.SpriteShader,
      MainFont,
      LoadingAtlas,
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
  var systemLoadTask: Task[Unit] = null

  override def load(): Unit = {
    LoadingState.assetBundle.load()
  }

  override def start(): Unit = {

    systemLoadTask = game.system.deferredLoad()
    loadingAssets = AssetLoader.startLoading()
    numAssetsBegin = loadingAssets.length
    numAssetsLeft = numAssetsBegin
    isLoading = true

    canvas.setLayerBlend(lSpinner, Renderer.BlendAddAlpha)
  }

  override def stop(): Unit = {
    loadingAssets = null

    AudioSystem.acquireHighLatency()

    // Collect garbage after loading
    System.gc()

    AudioSystem.releaseHighLatency()
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

    val time = AppWindow.currentTime

    renderer.beginFrame()
    renderer.setRenderTarget(RenderTarget.Backbuffer)
    renderer.clear(Some(Color.rgb(0x333333)), None)

    if (isLoading) {
      val numLoaded = numAssetsBegin - numAssetsLeft

      val frame = ((time * 20.0).toInt) % TurretSpinnerFill.length
      val fill = TurretSpinnerFill(frame)
      val outline = TurretSpinnerOutline(frame)


      val rotateTime = time * 0.5
      val pulseTime = time * 5.0
      val scale = 2.0

      val pulse = math.sin(pulseTime)
      var dx = pulse * math.cos(rotateTime) * scale
      val dy = pulse * math.sin(rotateTime) * scale

      val screen = Layout.screen720p
      val loadArea = screen.copy.padAround(50.0)
      val spinner = loadArea.copy.pushBottomLeft(100.0)

      val loadTexts = loadArea.copy.padTopLeft(25.0)
      val loadTitle = loadTexts.pushTop(50.0)
      val loadSubtitle1 = loadTexts.pushTop(30.0)
      val loadSubtitle2 = loadTexts.pushTop(30.0)

      canvas.draw(lMain, Background, screen)

      val spH = spinner.heightPx
      val spX = spinner.x0 - spH * 0.5
      val spY = spinner.y0

      canvas.draw(lSpinner, outline, spX - dx, spY - dy, 2.0 * spH, spH, Color.rgb(0xFF0000))
      canvas.draw(lSpinner, outline, spX, spY, 2.0 * spH, spH, Color.rgb(0x00FF00))
      canvas.draw(lSpinner, outline, spX + dx, spY + dy, 2.0 * spH, spH, Color.rgb(0x0000FF))

      canvas.drawText(lMain, tLoading, loadTitle, "Loading")
      canvas.drawText(lMain, tLoadInfo, loadSubtitle2, s"... assets: $numLoaded/$numAssetsBegin")
      canvas.drawText(lMain, tLoadInfo, loadSubtitle1, s"... frame: $frameCount")

      canvas.render()
    }

    LayoutDebugger.render()

    renderer.endFrame()

    AppWindow.swapBuffers()
  }

  override def done: Boolean = numAssetsLeft == 0 && isLoading

}
