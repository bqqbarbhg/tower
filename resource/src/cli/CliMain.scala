package cli

import java.nio.file.Paths

import core.StackAllocator
import io.SimpleSerialization.{SMap, SString}
import res.runner.{RunOptions, Runner}

/**
  * Main entry-point of the resource processer program.
  */
object CliMain extends App {

  val opts = new RunOptions()
  val arg = util.ArgumentParser.parse(args)

  /** Add the path of the config file to the value of `key` */
  private def resolveConfigPath(root: SMap, file: String, key: String): SMap = {
    root(key) match {
      case SString(str) =>
        val parent = Paths.get(file).toAbsolutePath.getParent
        val path = parent.resolve(str)
        root.updated(key, SString(path.normalize.toString))
      case _ => root
    }
  }

  // Read config from command line argument
  for (configFile <- arg.positional) {
    var config = io.Toml.parseFile(configFile)
    config = resolveConfigPath(config, configFile, "assetRoot")
    config = resolveConfigPath(config, configFile, "dataRoot")
    config = resolveConfigPath(config, configFile, "tempRoot")
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
