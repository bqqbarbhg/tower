package debug

import scala.collection.mutable

object ResourceTracker {
  var enabled: Boolean = true

  object DummyResource extends Resource("dummy", "", Array[StackTraceElement]())

  val resources = new mutable.HashSet[Resource]()

  private[debug] def add(kind: String, name: String = ""): Resource = {
    if (enabled) {
      val stack = Thread.currentThread.getStackTrace.drop(3)
      val res = new Resource(kind, name, stack)
      resources += res
      res
    } else {
      DummyResource
    }
  }

  private[debug] def remove(resource: Resource): Unit = {
    if (resource != DummyResource) {
      resources -= resource
    }
  }

  def active: Seq[Resource] = resources.toSeq

}


