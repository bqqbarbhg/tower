package main

object GameStartup {

  class Options {
    val engine: EngineStartup.Options = new EngineStartup.Options()
    var debug: Boolean = false
  }

  def start(opts: Options): Unit = {
    opts.engine.windowName = "Tower defence"
    EngineStartup.start(opts.engine)
  }

  def stop(): Unit = {
    EngineStartup.stop()
  }

}
