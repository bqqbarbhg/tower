package res

class RunOptions extends io.SimpleSerializable {
  var numThreads: Int = 0
  var numThreadsAuto: Boolean = true
  var assetRoot: String = "asset/"
  var dataRoot: String = "data/"
  var tempRoot: String = "temp/"

  def visit(v: io.SimpleVisitor): Unit = {
    numThreads = v.field("num-threads", numThreads)
    numThreadsAuto = v.field("num-threads-auto", numThreadsAuto)
    assetRoot = v.field("asset-root", assetRoot)
    dataRoot = v.field("data-root", dataRoot)
    tempRoot = v.field("temp-root", tempRoot)
  }
}

/**
  * The main resource processing runner.
  */
class Runner(val opts: RunOptions) {
}
