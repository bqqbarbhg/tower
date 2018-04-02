package game

import core._
import render._
import ui._
import locale._
import asset.{AssetLoader, FontAsset}
import main.EngineStartup
import platform.AppWindow
import io.content.{DirectoryPackage, Package}
import platform.AppWindow.WindowStyle
import ui.Canvas.TextStyle

object TestCanvas extends App {

  StackAllocator.createCurrentThread(16 * 1024 * 1024)
  Package.set(new DirectoryPackage("data"))

  LocaleInfo.load()

  {
    val code = Identifier("fi")
    locale.Locale.load(LocaleInfo.locales.find(_.code == code).getOrElse(LocaleInfo.locales.head))
  }

  AssetLoader.preloadAtlases()

  val opts = new EngineStartup.Options()
  opts.debug = true
  opts.windowName = "Sprite test"
  EngineStartup.start(opts)

  val windowStyle = new WindowStyle(1280, 720, false, false, -1, None)
  EngineStartup.softStart(windowStyle)

  val fontAsset = FontAsset("font/open-sans/OpenSans-Regular.ttf.s2ft")
  val style = TextStyle(fontAsset, 22.0)

  val canvas = new Canvas()

  var time = 0.0
  while (AppWindow.running) {
    time += 0.016
    AppWindow.pollEvents()

    val renderer = Renderer.get
    val viewWidth = AppWindow.width
    val viewHeight = AppWindow.height
    renderer.resizeBackbuffer(viewWidth, viewHeight)

    renderer.beginFrame()

    renderer.setRenderTarget(RenderTarget.Backbuffer)
    renderer.clear(Some(Color.rgb(0x6495ED)), None)
    renderer.setBlend(Renderer.BlendAlpha)

    canvas.render()

    renderer.endFrame()

    AppWindow.swapBuffers()
  }

  EngineStartup.softStop()
  EngineStartup.stop()
}

