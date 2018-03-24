package main

import game.options.GraphicsOptions.OpenGlOptions
import game.options.Options
import game.system.RenderingSystem
import gfx.OptsGfx
import io.SimpleSerialization.SMap
import io.Toml
import platform.AppWindow.WindowStyle
import render.opengl.{MapMode, OptsGl}

object GameStartup {

  var restartRequested: Boolean = false

  class Options {
    val engine: EngineStartup.Options = new EngineStartup.Options()
    var debug: Boolean = false
  }

  def start(opts: Options): Unit = {
    opts.engine.windowName = "Tower defence"
    EngineStartup.start(opts.engine)
    softStart()
  }

  private def softStart(): Unit = {
    Options.current = new game.options.Options()
    if (new java.io.File("options.toml").canRead) {
      val map = Toml.parseFile("options.toml")
      map.write(Options.current)
    }

    val opt = Options.current
    val gOpt = opt.graphics.quality
    val glOpt = opt.graphics.openGl
    OptsGl.uniformMap = OpenGlOptions.mapModeToEnum(glOpt.uniformMapMode, MapMode.PersistentCopy)
    OptsGl.vertexMap = OpenGlOptions.mapModeToEnum(glOpt.uniformMapMode, MapMode.Persistent)
    OptsGl.useUniformBlocks = glOpt.useUniformBuffers
    OptsGl.useVaoCache = glOpt.useVaoCache
    OptsGl.useTexStorage = glOpt.useImmutableTextureStorage
    OptsGl.useRowMajorMatrix = glOpt.useRowMajorMatrices
    OptsGfx.maxTextureSize = gOpt.maxTextureSize

    var swapInterval = 0
    if (gOpt.verticalSync) swapInterval = 1
    if (gOpt.halfFramerate) swapInterval = 2
    OptsGl.swapInterval = swapInterval

    val (fullscreen, borderless) = opt.windowMode match {
      case "Window" => (false, false)
      case "Fullscreen" => (true, true)
      case "Borderless" => (true, true)
    }

    val (resX, resY) = if (fullscreen) (0, 0) else (opt.windowSizeX, opt.windowSizeY)

    val windowStyle = new WindowStyle(resX, resY, fullscreen, borderless, opt.monitor)

    EngineStartup.softStart(windowStyle)
  }

  private def softStop(): Unit = {
    val map = SMap.read(Options.current)
    Toml.formatFile(map, "options.toml")

    RenderingSystem.unload()

    EngineStartup.softStop()
  }

  def stop(): Unit = {
    softStop()
    EngineStartup.stop()
  }

  def restart(): Unit = {
    softStop()
    softStart()
  }

}
