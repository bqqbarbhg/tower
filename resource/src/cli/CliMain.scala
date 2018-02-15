package cli

/**
  * Main entry-point of the resource processer program.
  */
object CliMain extends App {

  val opts = new res.RunOptions()
  val arg = util.ArgumentParser.parse(args)

  // Read config from command line argument
  for (configFile <- arg.positional.headOption) {
    val config = io.Toml.parseFile(configFile)
    config.write(opts)
  }

  // Command line options for changing the settings. Overrides file config.
  if (arg.flag("fast")) opts.skipOnTimestamp = true
  if (arg.flag("all")) opts.skipOnHash = false
  if (arg.flag("force")) opts.skipWriteOnHash = false
  if (arg.flag("verbose")) opts.verbose = true

  val runner = new res.Runner(opts)
  runner.run()
}
