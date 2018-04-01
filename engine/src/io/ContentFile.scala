package io

import java.nio.ByteBuffer

import core._
import util.BufferUtils._
import task.Task
import io.content.Package

import scala.collection.mutable.ArrayBuffer

object ContentFile {

  val BufferSize = 128 * 1024 * 1024
  val sharedBuffer = Memory.alloc(BufferSize)

  var lockingTask: Option[Task[_]] = None

  def load[T](name: String, userLoadFunc: ByteBuffer => T): Task[T] = load(Identifier(name), userLoadFunc)
  def load[T](name: Identifier, userLoadFunc: ByteBuffer => T): Task[T] = {
    val file = Package.get.get(name).getOrElse {
      throw new RuntimeException(s"Asset not found: $name")
    }

    def loadFileBuffer(): ByteBuffer = {
      val stream = file.read()
      val buffer = sharedBuffer.sliceEx
      buffer.readFrom(stream)
      buffer.finish()
      stream.close()
      buffer
    }

    val fileTask = Task.Io.addWithManualDependencies(lockingTask.size, loadFileBuffer)
    for (task <- lockingTask) task.linkDependent(fileTask)
    val userTask = Task.Main.add(fileTask, userLoadFunc)
    lockingTask = Some(userTask)

    userTask
  }

}


