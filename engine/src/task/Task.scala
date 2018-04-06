package task

import scala.collection.mutable.ArrayBuffer

object Task {

  val Main = new TaskExecutor()
  val Io = new TaskExecutor()
  val Worker = new TaskExecutor()

}

class Task[T](val executor: TaskExecutor, val dependencyCount: Int = 0, val fn: () => T, val debugName: String = "") {

  private[task] var result: T = _

  private var dependenciesLeft = dependencyCount
  private var completed: Boolean = false
  private val waiters = ArrayBuffer[Task[_]]()

  if (dependencyCount == 0)
    executor.schedule(this)

  private[task] def dependencyCompleted(): Unit = this.synchronized {
    dependenciesLeft -= 1
    if (dependenciesLeft == 0) {
      executor.schedule(this)
    }
  }

  def linkDependent(task: Task[_]): Unit = this.synchronized {
    if (completed)
      task.dependencyCompleted()
    else
      waiters += task
  }

  def run(): Unit = {
    result = fn()
    this.synchronized {
      completed = true
      for (w <- waiters)
        w.dependencyCompleted()
    }
  }

  def isCompleted: Boolean = completed

  private def waitForCompletion(): Unit = {

    // If this task belongs to another thread's executor add a dummy task to
    // the current thread's executor that depends on `this`. Now we can just
    // pump the tasks through the current thread until this is completed.
    if (!executor.isOnCurrentThread()) {
      TaskExecutor.forThisThread.add(this, (t: T) => { })
    }

    while (this.synchronized(!completed)) {
      TaskExecutor.forThisThread.runNextWait()
    }
  }

  def get: T = {
    if (this.synchronized(!completed)) {
      waitForCompletion()
    }
    result
  }

}
