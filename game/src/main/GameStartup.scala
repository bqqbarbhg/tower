package main

import game.options.GraphicsOptions.OpenGlOptions
import game.options.Options
import io.SimpleSerialization.SMap
import io.Toml
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
    val glOpt = opt.graphics.openGl
    OptsGl.uniformMap = OpenGlOptions.mapModeToEnum(glOpt.uniformMapMode, MapMode.PersistentCopy)
    OptsGl.vertexMap = OpenGlOptions.mapModeToEnum(glOpt.uniformMapMode, MapMode.Persistent)

    EngineStartup.softStart()
  }

  private def softStop(): Unit = {
    val map = SMap.read(Options.current)
    Toml.formatFile(map, "options.toml")

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
