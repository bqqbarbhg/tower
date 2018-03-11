package main

import core.StackAllocator

object EditorMain extends App {

  // Initialize 64MB stack
  StackAllocator.createCurrentThread(64 * 1024 * 1024)

  val arg = util.ArgumentParser.parse(args,
    implicitArgumentFlags = Vector("process"),
    multiArgumentFlags = Vector("process"),
    aliases = Map("P" -> "process"))

  val process = arg.multiKeywords("process")
  if (process.nonEmpty) {
    import res.runner.{RunOptions, Runner}
    val opts = RunOptions.createFromFiles(process)
    val runner = new Runner(opts)
    runner.run()
  }

  val opts = new GameStartup.Options()
  opts.engine.debug = arg.flag("debug")
  GameStartup.start(opts)
}
