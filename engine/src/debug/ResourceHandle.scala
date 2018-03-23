package debug

object ResourceHandle {

  def apply(kind: String, name: String = ""): Resource = {
    ResourceTracker.add(kind, name)
  }

}

class Resource private[debug] (val kind: String, val name: String, val stack: Array[StackTraceElement]) {
  def free(): Unit = ResourceTracker.remove(this)
}
