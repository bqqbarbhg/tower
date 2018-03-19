package game

import core._
import render._
import ui._
import locale._
import asset.{AssetLoader, FontAsset}
import main.EngineStartup
import platform.AppWindow
import io.content.{DirectoryPackage, Package}
import ui.Canvas.TextStyle

object TestCanvas extends App {

  StackAllocator.createCurrentThread(16 * 1024 * 1024)
  Package.set(new DirectoryPackage("data"))

  object LC {
    val Test = new LocaleGetter("Test") {
      private val key_long: Int = addSimple("long")
      def long: String = Locale.getSimple(key_long)

      private val key_hyphen: Int = addSimple("hyphen")
      def hyphen: String = Locale.getSimple(key_hyphen)
    }
  }

  LC
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
    renderer.setBlend(true)

    val yy = canvas.drawTextWrapped(0, style, Vector2(100.0, 100.0), Vector2(viewWidth - 200.0, 1000.0), LC.Test.long)
   canvas.drawTextWrapped(0, style, Vector2(100.0, 100.0 + yy + 20.0), Vector2(viewWidth - 200.0, 1000.0), LC.Test.hyphen)

    canvas.render()

    renderer.endFrame()

    AppWindow.swapBuffers()
  }

  EngineStartup.stop()
}

