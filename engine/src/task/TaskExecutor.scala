package task

import java.util.concurrent.LinkedBlockingQueue

object TaskExecutor {

  private val executorForThisThread = new ThreadLocal[TaskExecutor]

  def forThisThread: TaskExecutor = {
    val executor = executorForThisThread.get()
    assert(executor != null, "No executor set for current thread")
    executor
  }

}

class TaskExecutor {

  private val scheduledTasks = new LinkedBlockingQueue[Task[_]]()

  def schedule(task: Task[_]): Unit = {
    scheduledTasks.add(task)
  }

  /**
    * Potentially run a single task and return if there was a task to run.
    */
  def runNextTry(): Boolean = {
    val task = scheduledTasks.poll()
    if (task == null) return false
    task.run()
    true
  }

  /**
    * Run a single task and wait if no task is available.
    */
  def runNextWait(): Unit = {
    val task = scheduledTasks.take()
    task.run()
  }

  def addWithManualDependencies[R](numDependencies: Int, f: () => R, debugName: String = ""): Task[R] = {
    new Task[R](this, numDependencies, f, debugName)
  }

  def add[R](f: () => R): Task[R] = {
    new Task[R](this, 0, f)
  }

  def add[D1, R](d1: Task[D1], f: (D1) => R): Task[R] = {
    val task = new Task[R](this, 1, () => {
      f(d1.result)
    })

    d1.linkDependent(task)

    task
  }

  def add[D1, R](d1: Seq[Task[D1]], f: (Seq[D1]) => R): Task[R] = {
    val task = new Task[R](this, d1.length, () => {
      f(d1.map(_.result))
    })

    for (d <- d1) {
      d.linkDependent(task)
    }

    task
  }

  def add[D1, R](d1: Option[Task[D1]], f: (Option[D1]) => R): Task[R] = {
    val task = new Task[R](this, d1.size, () => {
      f(d1.map(_.result))
    })

    for (d <- d1) {
      d.linkDependent(task)
    }

    task
  }

  def add[D1, R](d1: Iterable[Task[D1]], f: (Iterable[D1]) => R): Task[R] = {
    val task = new Task[R](this, d1.size, () => {
      f(d1.map(_.result))
    })

    for (d <- d1) {
      d.linkDependent(task)
    }

    task
  }

  def isOnCurrentThread(): Boolean = TaskExecutor.executorForThisThread.get eq this

  def claimForThisThread(): Unit = {
    TaskExecutor.executorForThisThread.set(this)
  }
}

