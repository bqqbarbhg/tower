package cli

import core.StackAllocator
import res.runner.{RunOptions, Runner}

/**
  * Main entry-point of the resource processer program.
  */
object CliMain extends App {

  val opts = new RunOptions()
  val arg = util.ArgumentParser.parse(args)

  // Read config from command line argument
  for (configFile <- arg.positional.headOption) {
    val config = io.Toml.parseFile(configFile)
    config.write(opts)
  }

  // Command line options for changing the settings. Overrides file config.
  if (arg.flag("fast")) opts.skipOnTimestamp = true
  if (arg.flag("all")) {
    opts.skipOnTimestamp = false
    opts.skipOnHash = false
  }
  if (arg.flag("force")) opts.skipWriteOnHash = false
  if (arg.flag("verbose")) opts.verbose = true
  if (arg.flag("debug")) opts.debug = true

  // Initialize 64MB stack
  StackAllocator.createCurrentThread(64 * 1024 * 1024)

  val runner = new Runner(opts)
  runner.run()
}
