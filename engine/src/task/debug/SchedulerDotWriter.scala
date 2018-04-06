package task.debug

import task.{Task, TaskExecutor}

import scala.collection.mutable

class SchedulerDotWriter extends SchedulerDebugger {
  private val b = new StringBuilder()
  b.append("digraph g {\n")
  b.append("  rankdir = BT;\n")

  private val indexMap = mutable.HashMap[Task[Unit], Int]()
  private def taskId(task: Task[Unit]): Int = indexMap.getOrElseUpdate(task, indexMap.size + 1)

  def taskAdded(executor: TaskExecutor, task: Task[Unit], writeDep: Seq[AnyRef], readDep: Seq[AnyRef], writeDepTasks: Seq[Task[Unit]], readDepTasks: Seq[Task[Unit]]): Unit = {
    val id = taskId(task)
    b.append(s"""  $id [label="${task.debugName}"];\n""")
    for (dep <- writeDepTasks ++ readDepTasks) {
      val id2 = taskId(dep)
      b.append(s"  $id -> $id2;\n")
    }
  }

  lazy val result: String = {
    b.append("}\n")
    b.result
  }

}

