package main

import core.Identifier
import game.options.GraphicsOptions.OpenGlOptions
import game.options.Options
import game.system._
import gfx.OptsGfx
import io.SimpleSerialization.SMap
import io.Toml
import locale.{Locale, LocaleInfo}
import platform.AppWindow.WindowStyle
import platform.IconImage
import render.opengl.{MapMode, OptsGl}
import task.Task

object GameStartup {

  var restartRequested: Boolean = false
  var exitRequested: Boolean = false
  var iconTask: Task[IconImage] = null

  class Options {
    val engine: EngineStartup.Options = new EngineStartup.Options()
    var debug: Boolean = false
  }

  def start(opts: Options): Unit = {
    opts.engine.windowName = "Tower defence"
    EngineStartup.start(opts.engine)

    iconTask = IconImage.deferredLoad(Seq(
      Identifier("misc/icon/icon_128.png.s2tx"),
      Identifier("misc/icon/icon_64.png.s2tx"),
      Identifier("misc/icon/icon_48.png.s2tx"),
      Identifier("misc/icon/icon_32.png.s2tx"),
      Identifier("misc/icon/icon_24.png.s2tx"),
      Identifier("misc/icon/icon_16.png.s2tx"),
      Identifier("misc/icon/icon_8.png.s2tx"),
    ))

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

    val lcCode = Identifier(opt.language)
    val locale = LocaleInfo.locales.find(_.code == lcCode).getOrElse(LocaleInfo.defaultLocale)

    Locale.load(locale)

    val windowStyle = new WindowStyle(resX, resY, fullscreen, borderless, opt.monitor, Some(iconTask.get))

    EngineStartup.softStart(windowStyle)

    game.system.rendering.loadGlobal()
  }

  private def softStop(): Unit = {
    val map = SMap.read(Options.current)
    Toml.formatFile(map, "options.toml")

    rendering.unloadGlobal()
    audio.unloadGlobal()

    EngineStartup.softStop()
  }

  def stop(): Unit = {
    softStop()
    iconTask.get.unload()
    EngineStartup.stop()
    game.system.audio.audioSystem.joinAudioThread()
  }

  def restart(): Unit = {
    softStop()
    softStart()
  }

}
