package cli

/**
  * Main entry-point of the resource processer program.
  */
object CliMain extends App {

  val opts = new res.RunOptions()
  val arg = util.ArgumentParser.parse(args)

  // Read config from command line argument
  arg.positional.headOption match {
    case Some(configFile) =>
      val config = io.Toml.parseFile(configFile)
      config.write(opts)
    case None =>
  }

  // Command line options for changing the settings. Overrides file config.
  if (arg.flag("fast")) opts.skipOnTimestamp = true
  if (arg.flag("all")) opts.skipOnHash = false
  if (arg.flag("force")) opts.skipWriteOnHash = false

  val runner = new res.Runner(opts)
}
