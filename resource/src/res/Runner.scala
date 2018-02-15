package res

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

  def visit(v: io.SimpleVisitor): Unit = {
    numThreads = v.field("numThreads", numThreads)
    assetRoot = v.field("assetRoot", assetRoot)
    dataRoot = v.field("dataRoot", dataRoot)
    tempRoot = v.field("tempRoot", tempRoot)
    skipOnTimestamp = v.field("skipOnTimestamp", skipOnTimestamp)
    skipOnHash = v.field("skipOnHash", skipOnHash)
    skipWriteOnHash = v.field("skipWriteOnHash", skipWriteOnHash)
  }
}

/**
  * The main resource processing runner.
  */
class Runner(val opts: RunOptions) {
}
