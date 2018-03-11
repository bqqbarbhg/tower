package res.runner

import io.SimpleSerialization.{SMap, SString}
import java.nio.file.Paths

object RunOptions {

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

  /** Create `RunOptions` from a series of files */
  def createFromFiles(files: Iterable[String]): RunOptions = {
    val opts = new RunOptions()

    for (configFile <- files) {
      var config = io.Toml.parseFile(configFile)
      config = resolveConfigPath(config, configFile, "assetRoot")
      config = resolveConfigPath(config, configFile, "dataRoot")
      config = resolveConfigPath(config, configFile, "tempRoot")
      config.write(opts)
    }

    opts
  }
}

class RunOptions extends io.SimpleSerializable {

  /** Number of threads to process with, 0 for automatic */
  var numThreads: Int = 0

  /** Asset input directory */
  var assetRoot: String = "asset/"

  /** Data output directory */
  var dataRoot: String = "data/"

  /** Directory to store temporary files in, such as output file hashes */
  var tempRoot: String = "temp/"

  /** Skip processing files if the timestamps of the source files are older
    * than the destination files. Note: this is not reliable and should not
    * be used outside of developer environment. */
  var skipOnTimestamp: Boolean = false

  /** Skip processing files if the hashes of the source files haven't changed.
    * More heavy than `skipOnTimestamp`, but more reliable. */
  var skipOnHash: Boolean = true

  /** Skip writing output files if the hash differs from the _output_ file hash.
    * Saves SSD:s from wear, but should be disabled if you want to be 100% sure
    * that the new contents are saved. */
  var skipWriteOnHash: Boolean = true

  /** Print extra information on processed files */
  var verbose: Boolean = false
  /** Print extra debugging information */
  var debug: Boolean = false

  def visit(v: io.SimpleVisitor): Unit = {
    numThreads = v.field("numThreads", numThreads)
    assetRoot = v.field("assetRoot", assetRoot)
    dataRoot = v.field("dataRoot", dataRoot)
    tempRoot = v.field("tempRoot", tempRoot)
    skipOnTimestamp = v.field("skipOnTimestamp", skipOnTimestamp)
    skipOnHash = v.field("skipOnHash", skipOnHash)
    skipWriteOnHash = v.field("skipWriteOnHash", skipWriteOnHash)
    verbose = v.field("verbose", verbose)
    debug = v.field("debug", debug)
  }
}


