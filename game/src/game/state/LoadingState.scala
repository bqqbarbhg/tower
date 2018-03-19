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
      Font.FontShader,
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

  override def load(): Unit = {
    LoadingState.assetBundle.load()
  }

  override def start(): Unit = {
    loadingAssets = AssetLoader.startLoading()
    numAssetsBegin = loadingAssets.length
    numAssetsLeft = numAssetsBegin
    isLoading = true

    canvas.setLayerBlend(lSpinner, Renderer.BlendAddAlpha)
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

    val time = AppWindow.currentTime

    renderer.beginFrame()
    renderer.setRenderTarget(RenderTarget.Backbuffer)
    renderer.clear(Some(Color.rgb(0x333333)), None)

    if (isLoading) {
      val numLoaded = numAssetsBegin - numAssetsLeft

      val frame = ((time * 20.0).toInt) % TurretSpinnerFill.length
      val fill = TurretSpinnerFill(frame)
      val outline = TurretSpinnerOutline(frame)


      val colA = Color.rgb(0xFFAAAA)
      val colB = Color.rgb(0xAAAAFF)
      val outlineColor = Color.lerp(colA, colB, math.sin(time * 2.0 * math.Pi) * 0.5 + 0.5)
      val fillColor = (outlineColor * 0.5).copy(a = 0.2)


      val rotateTime = time * 0.5
      val pulseTime = time * 5.0
      val scale = 2.0

      val pulse = math.sin(pulseTime)
      var dx = pulse * math.cos(rotateTime) * scale
      val dy = pulse * math.sin(rotateTime) * scale

      canvas.draw(lMain, Background, 0.0, 0.0, 1280.0, 720.0)

      canvas.draw(lSpinner, outline, 0.0 - dx, 600.0 - dy, 200.0, 100.0, Color.rgb(0xFF0000))
      canvas.draw(lSpinner, outline, 0.0, 600.0, 200.0, 100.0, Color.rgb(0x00FF00))
      canvas.draw(lSpinner, outline, 0.0 + dx, 600.0 + dy, 200.0, 100.0, Color.rgb(0x0000FF))

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
