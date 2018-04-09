package task

import task.debug.SchedulerDebugger

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Scheduler {

  /** Currently attached debuggers */
  private var debuggers = Array[SchedulerDebugger]()

  /** Contains the latest write to a dependency */
  private val lastWrite = new mutable.HashMap[AnyRef, Task[Unit]]()

  /** Active read operations since the last write that reference a dependency.
    * The write itself counts as one of the read operations. */
  private val activeReads = new mutable.HashMap[AnyRef, Vector[Task[Unit]]]().withDefaultValue(Vector[Task[Unit]]())

  /** All tasks that have been added to the scheduler */
  private val allTasks = new ArrayBuffer[Task[Unit]]()

  /**
    * Attach a debugger to the scheduler that can listen to added tasks.
    *
    * @param debugger Debugger instance to add
    */
  def attachDebugger(debugger: SchedulerDebugger): Unit = {
    debuggers :+= debugger
  }

  /**
    * Add a task to the `Worker` executor. Equivalent to `addTo(Task.Worker)`.
    *
    * @param name Name used for debugging purposes
    * @param write Dependencies that can be written to and read from
    * @param read Dependencies that can only be read from
    * @param func Callback function to run
    * @see addTo() for more documentation
    */
  def add(name: String)(write: AnyRef*)(read: AnyRef*)(func: => Unit): Unit = addTo(name)(Task.Worker)(write: _*)(read: _*)(func)

  /**
    * Add a task to an executor. The task can have multiple dependencies that
    * may be optionally read-only. Writes are always sequentially ordered and
    * never overlap. Reads can never move past or overlap writes, but they are
    * unordered or potentially overlapping in regard to each other.
    *
    * @param name Name used for debugging purposes
    * @param executor Executor that will actually run the task
    * @param write Dependencies that can be written to and read from
    * @param read Dependencies that can only be read from
    * @param func Callback function to run
    */
  def addTo(name: String)(executor: TaskExecutor)(write: AnyRef*)(read: AnyRef*)(func: => Unit): Unit = {
    val writeDeps = write.flatMap(d => {
      val r = activeReads(d)
      // The dependency to the write is unnecessary in the presence of reads
      if (r.size > 1) r.drop(1) else r
    })
    val readDeps = read.flatMap(lastWrite.get)

    val numDeps = writeDeps.length + readDeps.length
    val task = executor.addWithManualDependencies(numDeps, () => func, name)
    allTasks += task

    for (debug <- debuggers) {
      debug.taskAdded(executor, task, write, read, writeDeps, readDeps)
    }

    for (dep <- writeDeps) dep.linkDependent(task)
    for (dep <- readDeps) dep.linkDependent(task)

    for (ref <- write) {
      activeReads(ref) = Vector(task)
      lastWrite(ref) = task
    }
    for (ref <- read) {
      activeReads(ref) :+= task
    }
  }

  /**
    * Shorthand for adding an unnamed task on the main thread with no dependencies.
    *
    * @param func Callback function to run on main thread
    */
  def defer(func: => Unit): Unit = {
    addTo("Defer")(Task.Main)()()(func)
  }

  /**
    * Return a task that completes when all tasks have finished.
    */
  def deferredFinish(): Task[Unit] = {
    val blockerTask = Task.Worker.addWithManualDependencies(allTasks.length, () => ())
    for (task <- allTasks) task.linkDependent(blockerTask)
    blockerTask
  }

  /**
    * Wait that all the tasks have finished.
    */
  def finish(): Unit = {
    deferredFinish().get
  }

}

