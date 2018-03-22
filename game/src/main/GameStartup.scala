package main

import game.options.Options
import io.SimpleSerialization.SMap
import io.Toml

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
  }

  private def softStop(): Unit = {
    val map = SMap.read(Options.current)
    Toml.formatFile(map, "options.toml")
  }

  def stop(): Unit = {
    EngineStartup.stop()
  }

  def restart(): Unit = {
    softStop()
    EngineStartup.restart()
    softStart()
  }

}
