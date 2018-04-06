package task.debug

import task.{Task, TaskExecutor}

trait SchedulerDebugger {
  def taskAdded(executor: TaskExecutor, task: Task[Unit], writeDep: Seq[AnyRef], readDep: Seq[AnyRef], writeDepTasks: Seq[Task[Unit]], readDepTasks: Seq[Task[Unit]]): Unit
}


