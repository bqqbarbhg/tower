package main

import render._
import core._
import asset.AssetLoader
import game.state._
import game.system.{ModelSystem, RenderingSystem}
import platform.AppWindow
import io.content._
import ui.Layout
import util.BufferUtils._

object EditorMain extends App {

  // Initialize 64MB stack
  StackAllocator.createCurrentThread(64 * 1024 * 1024)

  val arg = util.ArgumentParser.parse(args,
    implicitArgumentFlags = Vector("process"),
    multiArgumentFlags = Vector("process"),
    aliases = Map("P" -> "process"))

  def processResources(): Unit = {
    val process = arg.multiKeywords("process")
    if (process.nonEmpty) {
      import res.runner.{RunOptions, Runner}
      val opts = RunOptions.createFromFiles(process)
      val runner = new Runner(opts)
      runner.run()
    }
  }

  processResources()

  val pack = new MultiPackage()

  JarPackage.create("data") match {
    case Some(jar) => pack.add(jar, 1)
    case None => // Nop
  }

  pack.add(new DirectoryPackage("data"), 0)

  Package.set(pack)

  AssetLoader.preloadAtlases()

  val opts = new GameStartup.Options()
  opts.engine.debug = arg.flag("debug")
  opts.engine.glCompatability = arg.flag("gl-compat")
  GameStartup.start(opts)

  def dumpColorLookup(): Unit = {
    val exposure = 4.0

    val renderer = Renderer.get
    val readTarget = renderer.currentRenderTarget
    val w = readTarget.width
    val h = readTarget.height

    val writeTarget = RenderTarget.create(w, h, Some(TexFormat.Rgbf32), None, false)
    renderer.blitRenderTargetColor(writeTarget, readTarget)

    val floatPixels = Memory.alloc(w * h * 3 * 4)
    writeTarget.readColorPixels(0, floatPixels, TexFormat.Rgbf32)

    def putPixel(x: Int, y: Int, color: Color): Unit = {
      val base = ((h - y - 1) * w + x) * 3*4
      floatPixels.putFloat(base + 0, color.r.toFloat)
      floatPixels.putFloat(base + 4, color.g.toFloat)
      floatPixels.putFloat(base + 8, color.b.toFloat)
    }

    val LookupSize = 32

    def mapChannel(i: Int): Double = {
      val x = i.toDouble / (LookupSize - 1).toDouble
      x * x * exposure
    }

    for {
      r <- 0 until LookupSize
      g <- 0 until LookupSize
      b <- 0 until LookupSize
    } {
      val cr = mapChannel(r)
      val cg = mapChannel(g)
      val cb = mapChannel(b)
      putPixel(r + b * LookupSize, g, Color(cr, cg, cb))
    }

    val fixedPixels = Memory.alloc(w * h * 3 * 2)
    var y = 0
    while (y < h) {
      var src = (h - y - 1) * w * 3 * 4
      val srcEnd = src + w * 3 * 4
      while (src < srcEnd) {
        val f = floatPixels.getFloat(src)
        val i = clamp((f / exposure * 65535.0 + 0.5).toInt, 0, 0xFFFF)
        fixedPixels.putShort(i.toShort)
        src += 4
      }
      y += 1
    }

    fixedPixels.finish()
    val tiffBuffer = Memory.alloc(fixedPixels.capacity * 4)
    io.format.Tiff.writeLinearTiffRgb16(tiffBuffer, fixedPixels, w, h)

    tiffBuffer.finish()
    tiffBuffer.writeToFile("temp/screenshot_16.tiff")
    Memory.free(tiffBuffer)
    Memory.free(floatPixels)
    Memory.free(fixedPixels)
    writeTarget.unload()
  }

  do {
    GameStartup.restartRequested = false

    GameState.push(new MenuState())

    while (AppWindow.running && !GameStartup.restartRequested && !GameStartup.exitRequested) {
      RenderingSystem.updateScreenSize(AppWindow.width, AppWindow.height)
      GameState.update()

      if (AppWindow.keyEvents.exists(e => e.down == true && e.control && e.key == 'F')) {
        processResources()
        GameStartup.restartRequested = true
      }
      if (AppWindow.keyEvents.exists(e => e.down == true && e.control && e.key == 'R')) {
        processResources()
        AssetLoader.reloadEverything()
        ModelSystem.assetsLoaded()
      }
      if (AppWindow.keyEvents.exists(e => e.down == true && e.control && e.key == 'L')) {
        dumpColorLookup()
      }
      if (AppWindow.keyEvents.exists(e => e.down == true && e.control && e.key == 'U')) {
        Layout.debug = !Layout.debug
      }
    }

    if (GameStartup.restartRequested) {
      GameStartup.restart()
    }

  } while (GameStartup.restartRequested)

  GameStartup.stop()
}
