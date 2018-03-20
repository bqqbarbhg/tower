package game

import core._
import render._
import asset.AssetLoader
import main.EngineStartup
import platform.AppWindow
import ui.{Canvas, SpriteBatch}
import io.content.{DirectoryPackage, Package}
import locale.LocaleInfo


object TestSpriteBatch extends App {

  StackAllocator.createCurrentThread(16 * 1024 * 1024)
  Package.set(new DirectoryPackage("data"))
  AssetLoader.preloadAtlases()

  val opts = new EngineStartup.Options()
  opts.debug = true
  opts.windowName = "Sprite test"
  EngineStartup.start(opts)

  val sb = new SpriteBatch()

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

    {
      val sd = new SpriteBatch.SpriteDraw()
      sd.sprite = Identifier("sprites/a.png")
      sd.m11 = (math.cos(time) * 200.0).toFloat
      sd.m12 = (math.sin(time) * -200.0).toFloat
      sd.m21 = (math.sin(time) * 200.0).toFloat
      sd.m22 = (math.cos(time) * 200.0).toFloat
      sd.m13 = 100.0f
      sd.m23 = 100.0f
      sd.anchorX = 0.5f
      sd.anchorY = 0.5f
      sb.draw(sd)
    }

    val ff = (math.sin(time) * 0.5 + 0.5).toFloat

    {
      val sd = new SpriteBatch.SpriteDraw()
      sd.sprite = Identifier("sprites/a.png")
      sd.m11 = 100.0f
      sd.m22 = 100.0f
      sd.m13 = 300.0f
      sd.m23 = 200.0f
      sd.cropX0 = ff
      sb.draw(sd)
    }

    {
      val sd = new SpriteBatch.SpriteDraw()
      sd.sprite = Identifier("sprites/a.png")
      sd.m11 = 100.0f
      sd.m22 = 100.0f
      sd.m13 = 400.0f
      sd.m23 = 200.0f
      sd.cropY0 = ff
      sb.draw(sd)
    }

    {
      val sd = new SpriteBatch.SpriteDraw()
      sd.sprite = Identifier("sprites/a.png")
      sd.m11 = 100.0f
      sd.m22 = 100.0f
      sd.m13 = 400.0f
      sd.m23 = 300.0f
      sd.cropX1 = 1.0f - ff
      sb.draw(sd)
    }


    {
      val sd = new SpriteBatch.SpriteDraw()
      sd.sprite = Identifier("sprites/a.png")
      sd.m11 = 100.0f
      sd.m22 = 100.0f
      sd.m13 = 300.0f
      sd.m23 = 300.0f
      sd.cropY1 = 1.0f - ff
      sb.draw(sd)
    }

    sb.flush()

    renderer.endFrame()

    AppWindow.swapBuffers()
  }

  EngineStartup.stop()
}
