package main

import java.io.FileNotFoundException
import javax.swing.JOptionPane

import render._
import core._
import asset.AssetLoader
import game.state._
import platform.AppWindow
import io.content._
import ui.Layout
import util.BufferUtils._
import game.system.rendering._
import util.BufferIntegrityException

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

  for (error <- EngineStartup.verifyDataRoot()) {
    val builder = new StringBuilder()

    def write(line: String): Unit = {
      builder.append(line)
      builder.append('\n')
    }

    write("Failed to load game data")
    write(s"Error: ${error.getMessage}")

    error match {

      case ex: FileNotFoundException =>
        write("The game data was not found or is not processed. Make sure your working directory\n" +
              "is pointed at /tower-data/ and run the editor with the argument '-P config.toml'\n" +
              "to process the resources. If this doesn't solve the problem remove the /temp/ and\n" +
              "/data/ folders inside tower-data to re-process all the resources.")

        write("")

        val cwd = new java.io.File(".").getAbsolutePath.stripSuffix(".")

        write(s"Current working directory: $cwd")

      case ex: BufferIntegrityException =>
        write("The root data file exists but is corrupted. Try re-processing the resources.")

      case _ =>
    }

    val message = builder.result

    println(message)
    JOptionPane.showMessageDialog(null, message)
    System.exit(1)
  }

  AssetLoader.preloadAtlases()

  val opts = new GameStartup.Options()
  opts.engine.debug = arg.flag("debug")
  opts.engine.glCompatability = arg.flag("gl-compat")
  GameStartup.start(opts)

  def dumpColorLookup(): Unit = {
    val exposure = 4.0

    val renderer = Renderer.get
    val readTarget = globalRenderSystem.mainTargetMsaa
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

    if (false) {
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
      Memory.free(fixedPixels)
    } else {
      var ix = 0
      val num = w * h * 3
      while (ix < num) {
        val x = floatPixels.getFloat(ix * 4)
        val y = x / exposure
        floatPixels.putFloat(ix * 4, y.toFloat)
        ix += 1
      }

      val exrBuffer = Memory.alloc(floatPixels.capacity * 4)
      io.format.OpenExr.writeLinearOpenExrFloat32(exrBuffer, floatPixels, w, h)
      exrBuffer.finish()
      exrBuffer.writeToFile("temp/screenshot.exr")
      Memory.free(exrBuffer)
    }

    Memory.free(floatPixels)
    writeTarget.unload()
  }

  do {
    GameStartup.restartRequested = false

    GameState.push(new MenuState())

    while (AppWindow.running && !GameStartup.restartRequested && !GameStartup.exitRequested) {
      globalRenderSystem.updateScreenSize(AppWindow.width, AppWindow.height)
      GameState.update()

      if (AppWindow.keyEvents.exists(e => e.down == true && e.control && e.key == 'F')) {
        processResources()
        GameStartup.restartRequested = true
      }
      if (AppWindow.keyEvents.exists(e => e.down == true && e.control && e.key == 'R')) {
        processResources()
        AssetLoader.reloadEverything()
        modelSystem.assetsLoaded()
        animationSystem.assetsLoaded()
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
